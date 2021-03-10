package taskorganizer.webservice.TaskOrganizerWebService;

import com.ericsson.otp.erlang.*;
import org.json.JSONObject;

import javax.json.Json;
import javax.json.JsonObject;
import javax.json.bind.JsonbBuilder;
import javax.websocket.Session;
import java.io.IOException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import static java.lang.System.exit;

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

    public static void updateClients(OtpErlangObject update){
        System.out.println("forse faaaa: " + update);
        if (update instanceof OtpErlangTuple) {
            OtpErlangTuple tuple = (OtpErlangTuple) update;
            System.out.println("complete tuple: " + tuple);

            OtpErlangAtom op = (OtpErlangAtom) tuple.elementAt(0);

            ArrayList<Session> tmp = null;
            String payload = null;
            String boardTitle = null;
            // Check if correct message structure has been received
            if (op.atomValue().equals("ack_update_task")) {
                System.out.println("received ack_update_task");
                OtpErlangTuple taskTuple = (OtpErlangTuple) tuple.elementAt(1);

                boardTitle = ((OtpErlangString) taskTuple.elementAt(1)).stringValue();


                String taskTitle = ((OtpErlangString) taskTuple.elementAt(2)).stringValue();
                String destStage = taskTuple.elementAt(3).toString();

                System.out.println("title: " + taskTitle);
                System.out.println("dest stage: " + destStage);

                payload = new JSONObject()
                        .put("operation", "update_task")
                        .put("task_title", taskTitle)
                        .put("destination_stage", destStage)
                        .toString();

            }
            else if (op.atomValue().equals("ack_create_task")) {
                System.out.println("received ack_create_task");
                OtpErlangTuple taskTuple = (OtpErlangTuple) tuple.elementAt(1);

                boardTitle = ((OtpErlangString) taskTuple.elementAt(1)).stringValue();
                tmp = userBoards.get(boardTitle);

                String taskDescription = ((OtpErlangString) taskTuple.elementAt(2)).stringValue();
                String expirationDateString = ((OtpErlangString)taskTuple.elementAt(3)).stringValue();
                String stageIndex = taskTuple.elementAt(4).toString();
                String taskTitle = ((OtpErlangString) taskTuple.elementAt(5)).stringValue();
                String taskCreator = ((OtpErlangString) taskTuple.elementAt(6)).stringValue();
                String taskType = ((OtpErlangString) taskTuple.elementAt(7)).stringValue();

                payload = new JSONObject()
                        .put("operation", "create_task")
                        .put("task_description", taskDescription)
                        .put("expiration_date", expirationDateString)
                        .put("stage_index", stageIndex)
                        .put("task_title", taskTitle)
                        .put("task_creator", taskCreator)
                        .put("task_type", taskType)
                        .toString();

                //System.out.println(payload);
            }

            tmp = userBoards.get(boardTitle);
            try {
                // Send update message to each interested client
                for(Session session: tmp){
                    session.getBasicRemote().sendText(payload);
                }
            } catch (IOException e) {
                e.printStackTrace();
            }
        }else{
            System.out.println("Wrong message structure received from rabbitmq");
            exit(1);
        }
    }
}
