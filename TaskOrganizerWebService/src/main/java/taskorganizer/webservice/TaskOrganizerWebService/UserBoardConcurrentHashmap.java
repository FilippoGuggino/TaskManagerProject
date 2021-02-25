package taskorganizer.webservice.TaskOrganizerWebService;

import javax.websocket.Session;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

public class UserBoardConcurrentHashmap {
    // Key: board_title, Value: websocket sessions of users interested in that board
    // When a new update for a specific comes from the Erlang server, all users interested in that board
    // are informed of that update.
    private static final ConcurrentHashMap<String, ArrayList<Session>> userBoards = new ConcurrentHashMap<>();


    /**
     * When a new user connect to a board it is added to the "interested users" so that
     * each update received from this server for that specific board will be forwarded
     * to this client (it'll always have an up to date view of the board)
     *
     * @param board BoardTitle of which the user has put interest in
     * @param sessionWebSocket session of the websocket between the user and the server,
     *                         used to send and receive messages between each other
     */
    public static void add(String board, Session sessionWebSocket){
        // If the sessionId already exists it automatically updates only the value
        // If it is the first user interested in a specific board -> create new rabbitmq binding
        ArrayList<Session> interestedUsers;
        if(!userBoards.containsKey(board)) {
            interestedUsers = new ArrayList<>();
            interestedUsers.add(sessionWebSocket);
            RabbitMQManager.createBinding(board);
        }
        else{
            interestedUsers = userBoards.get(board);
            if(!interestedUsers.contains(sessionWebSocket)){
                interestedUsers.add(sessionWebSocket);
            }
        }
        userBoards.put(board, interestedUsers);
    }

    /**
     * When a user disconnect from the Board.jsp it is removed from the "interested"
     * users of that board. If no more user connected to this server are interested to
     * a specific board that also the RabbitMQ binding is removed(no more updates for
     * that specific board will be received from this server)
     *
     * @param sessionWebSocket WebSocketSession of the user who have disconnected
     */
    public static void remove(Session sessionWebSocket){
        Iterator<Map.Entry<String, ArrayList<Session>>> it = userBoards.entrySet().iterator();
        while(it.hasNext()){
            Map.Entry<String, ArrayList<Session>> pair = it.next();
            String boardTitle = pair.getKey();
            ArrayList<Session> listOfWebSocket = pair.getValue();
            if(listOfWebSocket.contains(sessionWebSocket)) {
                listOfWebSocket.remove(sessionWebSocket);
                if(listOfWebSocket.isEmpty()){
                    userBoards.remove(boardTitle);
                    RabbitMQManager.removeBinding(boardTitle);
                }
                else{
                    userBoards.put(boardTitle, listOfWebSocket);
                }
                break;
            }
        }
    }

    public static String printString(){
        return userBoards.toString();
    }
}
