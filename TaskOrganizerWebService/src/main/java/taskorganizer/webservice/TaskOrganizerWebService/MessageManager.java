package taskorganizer.webservice.TaskOrganizerWebService;
import com.ericsson.otp.erlang.*;

import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;

public class MessageManager {
    OtpNode webserver;
    OtpMbox mbox;

    public MessageManager(){
        try {
            this.webserver = new OtpNode("webservice@172.18.0.2", "test");
        } catch (Exception e) {
            e.printStackTrace();
        }
        this.mbox = this.webserver.createMbox("webserver");
        // TODO i don't think this is needed
        this.mbox.registerName("webserver");
        System.out.println("Connecting to the primary...");
        if (this.webserver.ping("erlang-server@172.18.0.162",2000)){
            System.out.println("OK: Primary server is online!");
        }
        else {
            // TODO WIP a new primary will probably be elected soon, a procedure to find the new primary needs to be employed
            System.out.println("WARNING: Primary server is offline!");
        }
    }
    public ArrayList<String> loadBoards() throws OtpErlangExit, OtpErlangDecodeException {
        OtpErlangObject[] msg = new OtpErlangObject[3];
        msg[0] = new OtpErlangAtom("load_boards");
        msg[1] = new OtpErlangAtom("primary");
        msg[2] = this.mbox.self();
        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);
        mbox.send("primary",formatted_msg);

        OtpErlangObject response;
        OtpErlangTuple boards;
        ArrayList<String> board_list = new ArrayList<>();
        response = this.mbox.receive();
        if (response instanceof OtpErlangTuple) {
            boards = (OtpErlangTuple)response;
            for (int i=0;i<boards.arity();i++){
                OtpErlangObject board_title = boards.elementAt(i);
                board_list.add(board_title.toString());
            }
        }
        return board_list;
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

                    Task task_element = new Task(single_task.elementAt(4).toString(),single_task.elementAt(6).toString(),
                            single_task.elementAt(1).toString(),exp_date,single_task.elementAt(5).toString());

                    String stage_id_s = single_task.elementAt(3).toString();
                    int stage_id = Integer.parseInt(stage_id_s);
                    stage_id = stage_id % 4;

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

    public boolean sendCreate(String board) throws OtpErlangExit, OtpErlangDecodeException {
        if(board.isEmpty()){
            System.err.println("Empty strings are not allowed for board name!");
            return false;
        }

        OtpErlangObject[] board_title = new OtpErlangObject[2];
        board_title[0] = new OtpErlangString(board);
        OtpErlangTuple formatted_board_title = new OtpErlangTuple(board_title);

        OtpErlangObject[] msg = new OtpErlangObject[5];
        msg[0] = new OtpErlangAtom("create_board");
        msg[1] = formatted_board_title;
        msg[2] = new OtpErlangAtom("primary");
        msg[4] = this.mbox.self();

        OtpErlangTuple formatted_msg = new OtpErlangTuple(msg);
        mbox.send("primary",formatted_msg);

        OtpErlangObject response;
        OtpErlangTuple response_mess;
        int timeout = 0;

        while(true) {
            response = this.mbox.receive();
            if (response instanceof OtpErlangTuple) {
                response_mess = (OtpErlangTuple) response;
                if (response_mess.elementAt(0).toString().equals("ack_create_board")) {
                    return true;
                } else {
                    if(timeout > 20){
                        return false;
                    }
                    this.mbox.send("primary",formatted_msg);
                    timeout++;
                }
            }
        }
    }

    public boolean sendMove(){
        //todo send message that a task has been moved from a stage to another
        return true;
    }

    public boolean sendDelete(){
        //todo send delete task message
        return true;
    }
}
