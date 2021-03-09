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
        String taskName = request.getParameter("task-name-mov");
        int taskStage = Integer.parseInt(request.getParameter("task-stage-mov"));
        String boardTitle = request.getParameter("selectBoard");

        try {
            MessageManager.sendMoveTask(boardTitle,taskName,taskStage);
        } catch (Exception e) {
            e.printStackTrace();
        }

        HttpSession session = request.getSession(true);
        String board_title = (String) session.getAttribute("currentBoard");
        request.setAttribute("selectBoard",  board_title);
        new BoardServlet().doGet(request, response);
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {

    }
}
