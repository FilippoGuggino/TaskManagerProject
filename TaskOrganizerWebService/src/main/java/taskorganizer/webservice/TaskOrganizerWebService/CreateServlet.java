package taskorganizer.webservice.TaskOrganizerWebService;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangString;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.annotation.*;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

@WebServlet(name = "CreateServlet", value = "/CreateServlet")
public class CreateServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String taskName = request.getParameter("task-name");
        String taskType = request.getParameter("task-type");
        String taskDesc = request.getParameter("task-desc");

        String exp_date_string = request.getParameter("task-exp");
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        Date taskExp = null;
        try {
            taskExp = formatter.parse(exp_date_string);
        } catch (ParseException e) {
            e.printStackTrace();
        }

        String taskCreator= request.getParameter("task-creator");

        Task task = new Task(taskName, taskType, taskDesc, taskExp, taskCreator);
        String boardTitle = request.getParameter("selectBoard");

        try {
            MessageManager.createTask(task, "Magicboard");
        } catch (Exception e) {
            e.printStackTrace();
        }

        response.sendRedirect("/TaskOrganizerWebService_war/Board?selectBoard="+boardTitle);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }
}
