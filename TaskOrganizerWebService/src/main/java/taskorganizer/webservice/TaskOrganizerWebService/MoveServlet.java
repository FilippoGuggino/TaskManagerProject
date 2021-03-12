package taskorganizer.webservice.TaskOrganizerWebService;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.annotation.*;
import java.io.IOException;
import java.text.ParseException;
import java.text.SimpleDateFormat;
import java.util.Date;

@WebServlet(name = "MoveServlet", value = "/MoveServlet")
public class MoveServlet extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String taskName = request.getParameter("task-name-move");
        int taskStageDestination = Integer.parseInt(request.getParameter("task-stage-mov-to"));

        String boardTitle = request.getParameter("selectBoard");

        try {
            MessageManager.sendMoveTask(boardTitle,taskName,taskStageDestination);
        } catch (Exception e) {
            e.printStackTrace();
        }

        response.sendRedirect("/TaskOrganizerWebService-1.0-SNAPSHOT/Board?selectBoard="+boardTitle);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }
}
