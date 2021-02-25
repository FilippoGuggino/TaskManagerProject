package taskorganizer.webservice.TaskOrganizerWebService;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.annotation.*;
import java.io.IOException;

@WebServlet(name = "MoveServlet", value = "/MoveServlet")
public class MoveServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String taskName = request.getParameter("task-name-mov");
        String taskStage = request.getParameter("task-stage-mov");
        //todo send move request
        //todo redirect to orginal page
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }
}
