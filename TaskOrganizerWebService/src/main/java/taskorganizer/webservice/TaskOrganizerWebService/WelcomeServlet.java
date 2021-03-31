package taskorganizer.webservice.TaskOrganizerWebService;

import com.ericsson.otp.erlang.OtpErlangDecodeException;
import com.ericsson.otp.erlang.OtpErlangExit;
import com.ericsson.otp.erlang.OtpNode;
import org.apache.log4j.LogManager;
import org.apache.log4j.Logger;

import java.io.*;
import java.util.ArrayList;
import javax.servlet.RequestDispatcher;
import javax.servlet.ServletException;
import javax.servlet.http.*;
import javax.servlet.annotation.*;

@WebServlet(name = "welcomeServlet", urlPatterns = "/welcome/*")
public class WelcomeServlet extends HttpServlet{

    public void init() {
    }

    public void doGet(HttpServletRequest request, HttpServletResponse response) throws IOException, ServletException {

        ArrayList<String> boards = new ArrayList<String>();

        boards = MessageManager.loadBoards();

        request.setAttribute("board_list",boards);
        RequestDispatcher rd = request.getRequestDispatcher("/index.jsp");
        rd.forward(request,response);
    }

    public void destroy() {
    }
}
