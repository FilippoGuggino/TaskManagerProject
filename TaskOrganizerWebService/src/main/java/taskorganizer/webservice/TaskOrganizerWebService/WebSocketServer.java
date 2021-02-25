package taskorganizer.webservice.TaskOrganizerWebService;

import javax.websocket.*;
import javax.websocket.server.ServerEndpoint;
import java.io.IOException;

/*
* This class works as an endpoint to all clients.
* When a client connect to a Board.jsp
* */
@ServerEndpoint("/websocketendpoint")
public class WebSocketServer {
    @OnOpen
    public void onOpen(Session session) throws IOException {
        System.out.println("Open Connection ...");
        System.out.println(session.getId());
    }

    @OnMessage
    public void onMessage(Session session, String message) throws IOException {
        UserBoardConcurrentHashmap.add(message, session);
        System.out.println("User is interested in this board: " + message);
        System.out.println("Interests of all clients: " + UserBoardConcurrentHashmap.printString());

    }

    @OnClose
    public void onClose(Session session){
        UserBoardConcurrentHashmap.remove(session);
        System.out.println("Close Connection ...");
        System.out.println("Interests of all clients: " + UserBoardConcurrentHashmap.printString());
    }

    @OnError
    public void onError(Throwable e){
        e.printStackTrace();
    }
}
