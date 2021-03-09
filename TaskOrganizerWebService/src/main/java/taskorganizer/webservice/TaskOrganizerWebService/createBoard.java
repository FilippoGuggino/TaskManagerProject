package taskorganizer.webservice.TaskOrganizerWebService;

import javax.servlet.*;
import javax.servlet.http.*;
import javax.servlet.annotation.*;
import java.io.IOException;

@WebServlet(name = "createBoard", value = "/createBoard")
public class createBoard extends HttpServlet {
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        String boardName = request.getParameter("board-name");

        try {
            MessageManager.sendCreateBoard(boardName);
        } catch (Exception e) {
            e.printStackTrace();
        }

//        HttpSession session = request.getSession(true);
//        session.setAttribute("currentBoard",boardName);
        request.setAttribute("selectBoard",  boardName);
        new BoardServlet().doGet(request, response);
    }
}
