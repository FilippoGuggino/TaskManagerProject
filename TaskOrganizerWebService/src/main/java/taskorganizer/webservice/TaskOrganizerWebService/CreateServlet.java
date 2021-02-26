package taskorganizer.webservice.TaskOrganizerWebService;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.annotation.*;
import java.io.IOException;
import java.util.Date;

@WebServlet(name = "CreateServlet", value = "/CreateServlet")
public class CreateServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String taskName = request.getParameter("task-name");
        String taskType = request.getParameter("task-desc");
        String taskDesc = request.getParameter("task-name");
        String taskExp = request.getParameter("task-exp");
        String taskCreator= request.getParameter("task-creator");

        System.out.println("ciiao");
        //todo send delete request to giuggio
        //todo redirect to original page
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }
}
