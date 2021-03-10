package taskorganizer.webservice.TaskOrganizerWebService;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;

import java.io.*;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "BoardServlet", value = "/Board")
public class BoardServlet extends HttpServlet{

    public void init() {
    }

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {

        UpdatePackage boardInfo = null;
        String board_title = request.getParameter("selectBoard");
        HttpSession session = request.getSession(true);
        if (board_title != null)
            session.setAttribute("currentBoard", board_title);
        else{
            board_title = (String) session.getAttribute("currentBoard");
        }

        try {
            System.out.println(board_title);
            boardInfo = MessageManager.loadTasks(board_title);
        } catch (OtpErlangDecodeException | OtpErlangExit e) {
            e.printStackTrace();
        }
//        String board = request.getParameter("selectBoard");

        ArrayList<Task> backlog_tasks = boardInfo.getBacklog();
        ArrayList<Task> doing_tasks = boardInfo.getDoing();
        ArrayList<Task> quality_check_tasks = boardInfo.getQualityCheck();
        ArrayList<Task> done_tasks = boardInfo.done_tasks;
//
//        //start test
//        String date_test = "2021-03-09";
//        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
//        Date date = new Date();
//        try {
//            date = formatter.parse(date_test);
//        } catch (ParseException e) {
//            e.printStackTrace();
//        }
//        Task Ciao = new Task("Test1","MANAGEMENT","Questa è una task di esempio",date,"admin");
//        backlog_tasks.add(Ciao);
//        doing_tasks.add(Ciao);
//        quality_check_tasks.add(Ciao);
//        done_tasks.add(Ciao);
        //end test

        //retrieve from sql database
        //todo

        //set attributes
        session.setAttribute("backlog_tasks",backlog_tasks);
        session.setAttribute("doing_tasks",doing_tasks);
        session.setAttribute("quality_check_tasks",quality_check_tasks);
        session.setAttribute("done_tasks",done_tasks);

        //forward request
        RequestDispatcher rd = request.getRequestDispatcher("/board.jsp");
        rd.forward(request,response);
    }

    public void destroy() {
    }
}