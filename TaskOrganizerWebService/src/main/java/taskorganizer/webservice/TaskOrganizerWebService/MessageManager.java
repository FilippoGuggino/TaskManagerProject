package taskorganizer.webservice.TaskOrganizerWebService;
import com.ericsson.otp.erlang.*;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.concurrent.ExecutionException;

import static java.lang.System.exit;

public class MessageManager {
    static String nameserver;
    static String erlangCookie = "test";
    static String erlangMailbox;
    static OtpNode webserver;
    static OtpMbox mbox;
    static OtpErlangPid primaryPid;
    public static String ip = "";

    static{
        resetErlangInterface();
    }

    private synchronized static void resetErlangInterface(){
        try {
            ip = InetAddress.getLocalHost().getHostAddress().toString();
            nameserver = "webservice@"+ip;
            erlangMailbox = "webmailbox-"+ip;
            webserver = new OtpNode(nameserver, erlangCookie);
        } catch (Exception e) {
            e.printStackTrace();
        }
        mbox = webserver.createMbox(erlangMailbox);
        mbox.registerName(nameserver);
        System.out.println("Connecting to the primary...");
        primaryPid = RabbitMQManager.fetchPrimary();
        System.out.println(primaryPid);
        if (webserver.ping(primaryPid.node(), 2000)){
            System.out.println("OK: Primary server is online!");
        }
        else {
            // TODO WIP a new primary will probably be elected soon, a procedure to find the new primary needs to be employed
            System.out.println("WARNING: Primary server is offline!");
        }
    }

    public synchronized static ArrayList<String> loadBoards(){
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("load_boards");
        msg[1] = new OtpErlangTuple(msg[0]);
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = mbox.self();
        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response = null;
        OtpErlangTuple boards = null;
        ArrayList<String> board_list = new ArrayList<>();
        response = sendAndWaitForResponse(formatted_msg);

        if(response == null){
            System.out.println("An error occurred while communicating with the primary");
            ArrayList tmp = new ArrayList<String>();
            tmp.add("dasdsa");
            return tmp;
        }

        if (response instanceof OtpErlangTuple) {
            OtpErlangTuple boardOuterTuple = (OtpErlangTuple) response;
            OtpErlangList boardErlangList = (OtpErlangList) boardOuterTuple.elementAt(0);
            OtpErlangTuple tmpTuple = null;
            OtpErlangString tmpBoardTitle = null;
            for (Iterator<OtpErlangObject> it = boardErlangList.iterator(); it.hasNext(); ) {
                tmpTuple = (OtpErlangTuple) it.next();
                tmpBoardTitle = (OtpErlangString) tmpTuple.elementAt(0);
                board_list.add(tmpBoardTitle.stringValue());
            }
        }
        return board_list;
    }

    public static void test(){
        OtpErlangObject[] msg = new OtpErlangObject[2];
        msg[0] = new OtpErlangAtom("test");
        msg[1] = mbox.self();
        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response = sendAndWaitForResponse(formatted_msg);
        OtpErlangTuple boards = null;

        if (response instanceof OtpErlangTuple) {
            boards = (OtpErlangTuple)response;
            OtpErlangAtom op = (OtpErlangAtom) boards.elementAt(0);
            System.out.println("atom: " + op.toString());

            OtpErlangList list = (OtpErlangList) boards.elementAt(1);
            System.out.println("list: " + list.toString());

            OtpErlangPid pid = (OtpErlangPid) boards.elementAt(2);
            System.out.println("pid: " + pid.toString());
        }

    }

    /**
     * This function send a generic message to the primary and wait for a response.
     * If no response is received then the primary will be deemed as "not reachable" and
     * rabbitmq will be used to fetch the PID of the new primary.
     * @param obj: Erlang object to send to the primary
     * @return : generic erlang object received from the primary
     */
    private synchronized static OtpErlangObject sendAndWaitForResponse(OtpErlangObject obj) {
        //System.out.println("send message: "+  obj.toString() +"   to " + primaryPid.toString());
        OtpErlangObject response = null;
        int i = 0;
        while(response == null){
            System.out.println("sending message to "+ primaryPid);
            mbox.send(primaryPid, obj);
            try {
                response = mbox.receive(2000);
                System.out.println("received response " + response);
                if(response == null){
                    System.out.println("Probably the primary is down... Fetching new primary info");
                    primaryPid = RabbitMQManager.fetchPrimary();
                }
            } catch (OtpErlangExit | OtpErlangDecodeException e) {
                e.printStackTrace();
            }

            i++;
            if(i == 3){
                System.out.println("An error occurred, primary seems unavailable, resetting connection...");
                webserver.close();
                mbox.close();
                resetErlangInterface();
                i = 0;
            }
        }
        System.out.println("Received response: " + response);
        return response;
    }

    /**
     * This function send a new message to the primary asking for all tasks belonging to a specified board.
     * Tasks are sent in an array format and each element has the following structure:
     * <<task_id, task_description, expiration_date, stage_id, title, creator, type>
     * @param board: String of the board name
     * @return an UpdatePackage that contains 4 arraylist, each one containing the tasks belonging to the
     * corresponding stage
     */
    public synchronized static UpdatePackage loadTasks(String board) throws OtpErlangExit, OtpErlangDecodeException {

        OtpErlangObject board_title = new OtpErlangString(board);
        OtpErlangTuple formatted_board_title = new OtpErlangTuple(board_title);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("load_tasks");
        msg[1] = formatted_board_title;
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = mbox.self();
        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        //send tasks request and wait for all tasks to arrive
        OtpErlangObject response = sendAndWaitForResponse(formatted_msg);
        OtpErlangTuple tasksTuple;

        //components of UpdatePackage
        ArrayList<Task> backlog_task_list = new ArrayList<>();
        ArrayList<Task> doing_task_list = new ArrayList<>();
        ArrayList<Task> quality_check_task_list = new ArrayList<>();
        ArrayList<Task> done_task_list = new ArrayList<>();

        if (response instanceof OtpErlangTuple) {

            tasksTuple = (OtpErlangTuple) response;
            OtpErlangList tasksList = (OtpErlangList) tasksTuple.elementAt(0);
            OtpErlangTuple singleTaskTuple = null;
            String taskTitle = null;
            String taskType = null;
            String taskDescription = null;
            String taskCreator = null;

            for (Iterator<OtpErlangObject> it = tasksList.iterator(); it.hasNext(); ) {

                singleTaskTuple = (OtpErlangTuple) it.next();

                String exp_date_string = ((OtpErlangString)singleTaskTuple.elementAt(2)).stringValue();
                SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
                Date exp_date;

                try {
                    exp_date = formatter.parse(exp_date_string);
                    taskTitle = ((OtpErlangString)singleTaskTuple.elementAt(4)).stringValue();
                    taskType = ((OtpErlangString)singleTaskTuple.elementAt(6)).stringValue();
                    taskDescription = ((OtpErlangString)singleTaskTuple.elementAt(1)).stringValue();
                    taskCreator = ((OtpErlangString)singleTaskTuple.elementAt(5)).stringValue();
                    // Received message format: Board_title, Description, expiration date, stage id, title, creator, type

                    // Task(String title, String type, String description, Date expiration, String creator)
                    Task task_element = new Task(taskTitle,
                            taskType,
                            taskDescription,
                            exp_date,
                            taskCreator);

                    String stageIdString = singleTaskTuple.elementAt(3).toString();

                    task_element.setStage_index(Integer.parseInt(stageIdString));

                    int stageId = Integer.parseInt(stageIdString);
                    switch (stageId){
                        case 0:
                            backlog_task_list.add(task_element);
                            break;
                        case 1:
                            doing_task_list.add(task_element);
                            break;
                        case 2:
                            quality_check_task_list.add(task_element);
                            break;
                        case 3:
                            done_task_list.add(task_element);
                            break;
                    }
                } catch (ParseException e) {
                    e.printStackTrace();
                }
            }
        }

        return new UpdatePackage(board,backlog_task_list,doing_task_list,
                quality_check_task_list,done_task_list);
    }

    /**
     * This function send a new message to the primary asking to create a new board with a specified name.
     * The function raise an exception when board name is empty, when ACK is incorrect and when the response message
     * is not an instance of an OtpErlangTuple.
     * @param board: String of the board name
     */

    public synchronized static void sendCreateBoard(String board) throws Exception {
        if(board == null){
            System.err.println("Empty strings are not allowed for board name!");
            throw new Exception("Empty strings are not allowed for board name");
        }

        OtpErlangString boardTitle = new OtpErlangString(board);

        //Message sent to Erlang {create_board, {"Board:Down secondary"}, primary, self()}
        OtpErlangTuple formatted_board_title = new OtpErlangTuple(boardTitle);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("create_board");
        msg[1] = formatted_board_title;
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = mbox.self();

        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);
//        mbox.send("primary",formatted_msg);

        OtpErlangObject response;
        OtpErlangTuple response_mess;

//        int timeout = 0;
        response = sendAndWaitForResponse(formatted_msg);

        if (response instanceof OtpErlangTuple) {
            response_mess = (OtpErlangTuple) response;
            if (response_mess.elementAt(0).toString().equals("ack_create_board")) {
                System.out.println("CREATE_BOARD: ACK correctly received");
                return;
            }
            else {
                System.out.println("CREATE_BOARD: Incorrect ACK received from the primary server!");
                throw new Exception(" Incorrect ACK");
            }
        }
        else {
            System.out.println("CREATE_BOARD: Response message is not an instance of OtpErlangTuple!");
            throw new Exception("Response message is not an instance of OtpErlangTuple");
        }

        //OLD CODE - still needs to see if new one is ok, do not delete it please
//        while(true) {
//            if (response instanceof OtpErlangTuple) {
//                response_mess = (OtpErlangTuple) response;
//                if (response_mess.elementAt(0).toString().equals("ack_create_board")) {
//                    return true;
//                }
//                else {
//                    if(timeout > 20){
//                        return false;
//                    }
//                    timeout++;
//                    response = sendAndWaitForResponse(formatted_msg);
//                }
//            }
//        }
    }

    /**
     * This function send a new message to the primary asking to create a new task in a specific board with specified parameters.
     * The function raise an exception when board name is empty, when ACK is incorrect and when the response message
     * is not an instance of an OtpErlangTuple.
     * @param task: Task object that contains all the parameters of the task that needs to be created
     * @param board: String of the board name
     */

    public synchronized static void createTask(Task task, String board) throws Exception {

        if(board.isEmpty()){
            System.err.println("CREATE TASK: Empty strings are not allowed for board name!");
            throw new Exception("Empty strings are not allowed for board name");
        }

        // Send this message format: Description, expiration date, stage id, title, creator, type

        OtpErlangObject[] Task = new OtpErlangObject[7];
        Task[0] = new OtpErlangString(board);
        Task[1] = new OtpErlangString(task.isDescription());
        Task[2] = new OtpErlangString(task.expirationDate());
        Task[3] = new OtpErlangInt(task.currentStage());
        Task[4] = new OtpErlangString(task.isTitle());
        Task[5] = new OtpErlangString(task.isCreator());
        Task[6] = new OtpErlangString(task.isType());
        OtpErlangTuple formatted_task = new OtpErlangTuple(Task);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("create_task");
        msg[1] = formatted_task;
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = mbox.self();

        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response;
        OtpErlangTuple response_mess;

        response = sendAndWaitForResponse(formatted_msg);

        if (response instanceof OtpErlangTuple) {
            response_mess = (OtpErlangTuple) response;
            if (response_mess.elementAt(0).toString().equals("ack_create_task")) {
                System.out.println("CREATE_TASK: ACK correctly received");
                //TODO need to check task title?
                return;
            }
            else {
                System.err.println("CREATE_TASK: Incorrect ACK received from the primary server!");
                throw new Exception("Incorrect ACK");
            }
        }
        else {
            System.err.println("CREATE_TASK: Response message is not an instance of OtpErlangTuple!");
            throw new Exception("Response message is not an instance of OtpErlangTuple");
        }

    }

    /**
     * This function send a new message to the primary asking to move new task in a specific board to new stage.
     * The function raise an exception when board name is empty, when ACK is incorrect and when the response message
     * is not an instance of an OtpErlangTuple.
     * @param board: String of the board name
     * @param taskTitle: Title of the task that has to be inserted
     * @param toStage: integer that specify the number of the new stage
     */

    public synchronized static void sendMoveTask(String board, String taskTitle, int toStage) throws Exception {

        if(board.isEmpty()){
            System.err.println("MOVE_TASK: Empty strings are not allowed for board name!");
            throw new Exception("Empty strings are not allowed for board name");
        }

        // Send this message format: board, task title, new stage id

        OtpErlangObject[] Task = new OtpErlangObject[3];
        Task[0] = new OtpErlangString(board);
        Task[1] = new OtpErlangString(taskTitle);
        Task[2] = new OtpErlangInt(toStage);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("update_task");
        msg[1] = new OtpErlangTuple(Task);
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = mbox.self();

        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response;
        OtpErlangTuple response_mess;

        response = sendAndWaitForResponse(formatted_msg);

        if (response instanceof OtpErlangTuple) {
            response_mess = (OtpErlangTuple) response;
            if (response_mess.elementAt(0).toString().equals("ack_update_task")) {
                OtpErlangTuple updateInfo = (OtpErlangTuple) response_mess.elementAt(1);
                taskTitle = ((OtpErlangString)updateInfo.elementAt(2)).stringValue();
                if (taskTitle.equals(taskTitle)){
                    System.out.println("MOVE_TASK: ACK and task title correctly received");
                    return;
                }
                else {
                    System.err.println("MOVE_TASK: Incorrect task title from the primary server!");
                    throw new Exception("Incorrect task title from the primary server");
                }
            }
            else {
                System.err.println("MOVE_TASK: Incorrect ACK received from the primary server!");
                throw new Exception("Empty strings are not allowed for board name");
            }
        }
        else {
            System.err.println("MOVE_TASK: Response message is not an instance of OtpErlangTuple!");
            throw new Exception("Response message is not an instance of OtpErlangTuple");
        }
    }

    public static boolean sendDelete(){
        //todo send delete task message
        return true;
    }
}
