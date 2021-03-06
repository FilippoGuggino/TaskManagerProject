package taskorganizer.webservice.TaskOrganizerWebService;
import com.ericsson.otp.erlang.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

import static java.lang.System.exit;

public class MessageManager {
    OtpNode webserver;
    OtpMbox mbox;
    OtpErlangPid primaryPid;

    public MessageManager(){
        try {
            this.webserver = new OtpNode("webservice@172.18.0.2", "test");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.mbox = this.webserver.createMbox("webserver");
        this.mbox.registerName("webserver");
        System.out.println("Connecting to the primary...");
        this.primaryPid = RabbitMQManager.fetchPrimary();
        if (this.webserver.ping(this.primaryPid.node(), 2000)){
            System.out.println("OK: Primary server is online!");
        }
        else {
            // TODO WIP a new primary will probably be elected soon, a procedure to find the new primary needs to be employed
            System.out.println("WARNING: Primary server is offline!");
        }
    }

    public ArrayList<String> loadBoards() throws OtpErlangDecodeException {
        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("load_boards");
        msg[1] = new OtpErlangAtom("load_boards");
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = this.mbox.self();
        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response = null;
        OtpErlangTuple boards = null;
        ArrayList<String> board_list = new ArrayList<>();
        response = sendAndWaitForResponse(formatted_msg);

        if(response == null){
            System.out.println("An error occurred while communicating with the primary");
            exit(1);
        }

        if (response instanceof OtpErlangTuple) {
            boards = (OtpErlangTuple)response;
            for (int i=0;i<boards.arity();i++){
                OtpErlangObject board_title = boards.elementAt(i);
                board_list.add(board_title.toString());
            }
        }
        return board_list;
    }

    public void test(){
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
    private OtpErlangObject sendAndWaitForResponse(OtpErlangObject obj) {
        System.out.println("send message: "+  obj.toString() +"   to " + primaryPid.toString());
        OtpErlangObject response = null;
        while(response == null){
            this.mbox.send(primaryPid, obj);
            try {
                response = this.mbox.receive(2000);
                if(response == null){
                    System.out.println("Probably the primary is down... Fetching new primary info");
                    this.primaryPid = RabbitMQManager.fetchPrimary();
                }
            } catch (OtpErlangExit | OtpErlangDecodeException e) {
                e.printStackTrace();
            }
        }
        System.out.println("received response");
        return response;
    }

    public UpdatePackage loadTasks(String board) throws OtpErlangExit, OtpErlangDecodeException {

        OtpErlangObject[] board_title = new OtpErlangObject[2];
        board_title[0] = new OtpErlangString(board);
        OtpErlangTuple formatted_board_title = new OtpErlangTuple(board_title);

        OtpErlangObject[] msg = new OtpErlangObject[5];
        msg[0] = new OtpErlangAtom("load_tasks");
        msg[1] = formatted_board_title;
        msg[2] = new OtpErlangAtom("primary");
        msg[4] = this.mbox.self();
        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);
        mbox.send("primary",formatted_msg);

        OtpErlangObject response;
        OtpErlangTuple tasks;

        ArrayList<Task> backlog_task_list = new ArrayList<>();
        ArrayList<Task> doing_task_list = new ArrayList<>();
        ArrayList<Task> quality_check_task_list = new ArrayList<>();
        ArrayList<Task> done_task_list = new ArrayList<>();

        response = this.mbox.receive();

        if (response instanceof OtpErlangTuple) {
            tasks = (OtpErlangTuple)response;
            for (int i=0;i<tasks.arity();i++){
                OtpErlangTuple single_task = (OtpErlangTuple) tasks.elementAt(i);

                String exp_date_string = single_task.elementAt(2).toString();
                SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
                Date exp_date;

                try {

                    exp_date = formatter.parse(exp_date_string);

                    // Received message format: task id, Description, expiration date, stage id, title, creator, type

                    Task task_element = new Task(single_task.elementAt(4).toString(),single_task.elementAt(6).toString(),
                            single_task.elementAt(1).toString(),exp_date,single_task.elementAt(5).toString());

                    //set task id for database purposes
                    task_element.setID(Integer.parseInt(single_task.elementAt(0).toString()));

                    String stage_id_s = single_task.elementAt(3).toString();
                    int stage_id = Integer.parseInt(stage_id_s);

                    //setting integral stage number for database purposes
                    task_element.setIntegralStage(stage_id);

                    stage_id = stage_id % 4;

                    task_element.setStage_index(stage_id);

                    if (stage_id == 0){
                        backlog_task_list.add(task_element);
                    }
                    if (stage_id == 1){
                        doing_task_list.add(task_element);
                    }
                    if (stage_id == 2){
                        quality_check_task_list.add(task_element);
                    }
                    if (stage_id == 3){
                        done_task_list.add(task_element);
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

    public void sendCreateBoard(String board) throws Exception {
        if(board.isEmpty()){
            System.err.println("Empty strings are not allowed for board name!");
            throw new Exception("Empty strings are not allowed for board name");
        }

        OtpErlangObject[] board_title = new OtpErlangObject[2];
        board_title[0] = new OtpErlangString(board);
        OtpErlangTuple formatted_board_title = new OtpErlangTuple(board_title);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("create_board");
        msg[1] = formatted_board_title;
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = this.mbox.self();

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

    public int sendCreateTask(Task task, String board) throws Exception {

        if(board.isEmpty()){
            System.err.println("CREATE TASK: Empty strings are not allowed for board name!");
            throw new Exception("Empty strings are not allowed for board name");
        }

        // Send this message format: Description, expiration date, stage id, title, creator, type

        OtpErlangObject[] Task = new OtpErlangObject[7];
        Task[0] = new OtpErlangString(board);
        Task[1] = new OtpErlangString(task.isDescription());
        Task[2] = new OtpErlangString(task.expirationDate());
        Task[3] = new OtpErlangString(Integer.toString(task.currentStage()));
        Task[4] = new OtpErlangString(task.isTitle());
        Task[5] = new OtpErlangString(task.isCreator());
        Task[6] = new OtpErlangString(task.isType());
        OtpErlangTuple formatted_task = new OtpErlangTuple(Task);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("create_task");
        msg[1] = formatted_task;
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = this.mbox.self();

        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response;
        OtpErlangTuple response_mess;

        response = sendAndWaitForResponse(formatted_msg);

        if (response instanceof OtpErlangTuple) {
            response_mess = (OtpErlangTuple) response;
            if (response_mess.elementAt(0).toString().equals("ack_create_task")) {
                System.out.println("CREATE_TASK: ACK correctly received");
                return Integer.parseInt(response_mess.elementAt(2).toString());
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
     * @param taskID: ID of the task that has to be inserted
     * @param toStage: integer that specify the number of the new stage
     */

    public void sendMoveTask(String board, int taskID, int toStage, String desc) throws Exception {

        if(board.isEmpty()){
            System.err.println("MOVE_TASK: Empty strings are not allowed for board name!");
            throw new Exception("Empty strings are not allowed for board name");
        }

        // Send this message format: board, task id, new stage id

        OtpErlangObject[] Task = new OtpErlangObject[4];
        Task[0] = new OtpErlangString(board);
        Task[1] = new OtpErlangString(Integer.toString(taskID));
        Task[2] = new OtpErlangString(Integer.toString(toStage));

        //TODO remove this last parameter called new_task_description
        Task[3] = new OtpErlangString(desc);

        OtpErlangTuple formatted_move = new OtpErlangTuple(Task);

        OtpErlangObject[] msg = new OtpErlangObject[4];
        msg[0] = new OtpErlangAtom("update_task");
        msg[1] = formatted_move;
        msg[2] = new OtpErlangAtom("primary");
        msg[3] = this.mbox.self();

        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);

        OtpErlangObject response;
        OtpErlangTuple response_mess;

        response = sendAndWaitForResponse(formatted_msg);

        if (response instanceof OtpErlangTuple) {
            response_mess = (OtpErlangTuple) response;
            if (response_mess.elementAt(0).toString().equals("ack_update_task")) {
                if (response_mess.elementAt(1).toString().equals(String.valueOf(taskID))){
                    System.out.println("MOVE_TASK: ACK and TaskID correctly received");
                    return;
                }
                else {
                    //TODO check id we need to cycle here
                    System.err.println("MOVE_TASK: Incorrect TaskID from the primary server!");
                    throw new Exception("Incorrect TaskID from the primary server");
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

    public boolean sendDelete(){
        //todo send delete task message
        return true;
    }
}
