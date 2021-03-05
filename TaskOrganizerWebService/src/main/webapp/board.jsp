<%@ page import="java.util.ArrayList" %>
<%@ page import="taskorganizer.webservice.TaskOrganizerWebService.Task" %>
<%@ page import="java.util.Date" %>
<%@ page import="java.text.SimpleDateFormat" %><%--
  Created by IntelliJ IDEA.
  User: Leonardo
  Date: 10/02/2021
  Time: 17:09
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<link rel="stylesheet" href="css/board.css">
<head>
    <title>Board overview</title>
    <script>
        function updateBoardManager() {
            var serverSocket = new WebSocket("ws://localhost:8080/TaskOrganizerWebService_war/websocketendpoint");
            var selectedBoard = findGetParameter("selectBoard");


            // Receives update from server
            serverSocket.onmessage = function(event)
            {
                alert(event.data);
                // TODO update task according to the message
            }

            // Send the selected board to the server (meaning that I am interested
            // in updates regarding this board)
            serverSocket.onopen = function (event){
                serverSocket.send(selectedBoard);
            }
        }

        function findGetParameter(parameterName) {
            var result = null,
                tmp = [];
            var items = location.search.substr(1).split("&");
            for (var index = 0; index < items.length; index++) {
                tmp = items[index].split("=");
                if (tmp[0] === parameterName) result = decodeURIComponent(tmp[1]);
            }
            return result;
        }
    </script>
</head>
<body onload="updateBoardManager();">

<h1 id="title_1">Task</h1>
<h1 id="title_2"> Organizer</h1>

<h1 id="board_title">Your board</h1>

<div id="main-frame">
    <%
        out.println("<h2 id=\"current_board\">"+session.getAttribute("currentBoard")+"</h2>");
    %>
    <div id="backlog-stage">
        <p class="stage_title">BACKLOG</p>
        <%
            ArrayList<Task> backlog_tasks = (ArrayList<Task>) session.getAttribute("backlog_tasks");
            if (backlog_tasks.size()>0) {
                for (int i = 0; i < backlog_tasks.size(); i++) {
                    String color;
                    if (backlog_tasks.get(i).isExpired()) {
                        color = "red";
                    } else {
                        color = "green";
                    }
                    out.println("<div class=\"task_div\" style=\"border-color:" + color + ";\">");
                    out.println("<h1 class=\"title\">" + backlog_tasks.get(i).isTitle() + "</h1>");
                    out.println("<h2 class=\"type\">" + backlog_tasks.get(i).isType() + "</h2>");
                    out.println("<p class=\"desc\">" + backlog_tasks.get(i).isDescription() + "</p>");
                    out.println("<p class=\"created\">Created by: " + backlog_tasks.get(i).isCreator() + "</p>");
                    out.println("<p class=\"expiration\">Expires on: " + backlog_tasks.get(i).expirationDate() + "</p>");
                    out.println("</div>");
                }
            }
            else {
                out.println("No tasks");
            }
        %>
    </div>
    <div id="doing-stage">
        <p class="stage_title">DOING</p>
        <%
            ArrayList<Task> doing_tasks = (ArrayList<Task>) session.getAttribute("doing_tasks");
            if (doing_tasks.size()>0) {
                for (int i = 0; i < doing_tasks.size(); i++) {
                    String color;
                    if (doing_tasks.get(i).isExpired()) {
                        color = "red";
                    } else {
                        color = "green";
                    }
                    out.println("<div class=\"task_div\" style=\"border-color:" + color + ";\">");
                    out.println("<h1 class=\"title\">" + doing_tasks.get(i).isTitle() + "</h1>");
                    out.println("<h2 class=\"type\">" + doing_tasks.get(i).isType() + "</h2>");
                    out.println("<p class=\"desc\">" + doing_tasks.get(i).isDescription() + "</p>");
                    out.println("<p class=\"created\">Created by: " + doing_tasks.get(i).isCreator() + "</p>");
                    out.println("<p class=\"expiration\">Expires on: " + doing_tasks.get(i).expirationDate() + "</p>");
                    out.println("</div>");

                }
            }
            else {
                out.println("No tasks");
            }
        %>
    </div>
    <div id="quality-check-stage">
        <p class="stage_title">QUALITY CHECK</p>
        <%
            ArrayList<Task> quality_check_tasks = (ArrayList<Task>) session.getAttribute("quality_check_tasks");
            if (quality_check_tasks.size() > 0) {
                for (int i = 0; i < quality_check_tasks.size(); i++) {
                    String color;
                    if (quality_check_tasks.get(i).isExpired()) {
                        color = "red";
                    } else {
                        color = "green";
                    }
                    out.println("<div class=\"task_div\" style=\"border-color:" + color + ";\">");
                    out.println("<h1 class=\"title\">" + quality_check_tasks.get(i).isTitle() + "</h1>");
                    out.println("<h2 class=\"type\">" + quality_check_tasks.get(i).isType() + "</h2>");
                    out.println("<p class=\"desc\">" + quality_check_tasks.get(i).isDescription() + "</p>");
                    out.println("<p class=\"created\">Created by: " + quality_check_tasks.get(i).isCreator() + "</p>");
                    out.println("<p class=\"expiration\">Expires on: " + quality_check_tasks.get(i).expirationDate() + "</p>");
                    out.println("</div>");
                }
            }
            else {
                out.println("No tasks");
            }
        %>
    </div>
    <div id="done-stage">
        <p class="stage_title">DONE</p>
        <%
            ArrayList<Task> done_tasks = (ArrayList<Task>) session.getAttribute("done_tasks");
            if (done_tasks.size()>0) {
                for (int i = 0; i < done_tasks.size(); i++) {
                    String color;
                    if (done_tasks.get(i).isExpired()) {
                        color = "red";
                    } else {
                        color = "green";
                    }
                    out.println("<div class=\"task_div\" style=\"border-color:" + color + ";\">");
                    out.println("<h1 class=\"title\">" + done_tasks.get(i).isTitle() + "</h1>");
                    out.println("<h2 class=\"type\">" + done_tasks.get(i).isType() + "</h2>");
                    out.println("<p class=\"desc\">" + done_tasks.get(i).isDescription() + "</p>");
                    out.println("<p class=\"created\">Created by: " + done_tasks.get(i).isCreator() + "</p>");
                    out.println("<p class=\"expiration\">Expires on: " + done_tasks.get(i).expirationDate() + "</p>");
                    out.println("</div>");

                }
            }
            else {
                out.println("No tasks");
            }
        %>
    </div>
    <br>
    <div id="op_menu">
        <div id="creation-menu">
            <%
                SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
                Date currentDate = new Date();
            %>
            <form id="creation-form" action="CreateServlet">
                <h2>Create a new task</h2>

                <label for="task-name">Task name</label>
                <input type="text" placeholder="Enter task name" name="task-name" id="task-name" required>

                <label for="task-type">Task type</label>
                <select name="task-type" id="task-type">
                    <option value="MANAGEMENT">MANAGEMENT</option>
                    <option value="REQUIREMENTS">REQUIREMENTS</option>
                    <option value="DESIGN">DESIGN</option>
                    <option value="IMPLEMENTATION">IMPLEMENTATION</option>
                    <option value="TESTING">TESTING</option>
                </select>

                <label for="task-desc">Task description</label>
                <input type="text" placeholder="Enter task description" name="task-desc" id="task-desc" required>

                <%
                    out.print("<label for='task-exp'>Task expiration date</label>");
                    out.println("<input type=\"date\" name=\"task-exp\" id=\"task-exp\" " +
                            "value="+formatter.format(currentDate)+" min="+formatter.format(currentDate)+">");
                %>

                <label for="task-creator">Task creator</label>
                <input type="text" placeholder="Enter creator's name" name="task-creator" id="task-creator" required>

                <br><br>
                <input class="submit" type="submit" value="Create">

            </form>
        </div>

        <div id="deletion-menu">
            <form id="deletion-form" action="DeleteServlet">
                <h2>Delete a task</h2>

                <label for="task-name-del">Task name</label>
                <input type="text" placeholder="Enter task name" name="task-name-del" id="task-name-del" required>

                <label for="task-stage-del">Select to which stage</label>
                <select name="task-stage-del" id="task-stage-del">
                    <option value="0">BACKLOG</option>
                    <option value="1">DOING</option>
                    <option value="2">QUALITY CHECK</option>
                    <option value="3">DONE</option>
                </select>

                <br><br>
                <input class="submit" type="submit" value="Submit">

            </form>
        </div>

        <div id="move-menu">
            <form id="move-form" action="MoveServlet">
                <h2>Move a task</h2>

                <label for="task-name-move">Task name</label>
                <input type="text" placeholder="Enter task name" name="task-name-move" id="task-name-move" required>

                <label for="task-stage-mov-from">Select from which stage</label>
                <select name="task-stage-mov-from" id="task-stage-mov-from">
                    <option value="0">BACKLOG</option>
                    <option value="1">DOING</option>
                    <option value="2">QUALITY CHECK</option>
                    <option value="3">DONE</option>
                </select>

                <label for="task-stage-mov-to">Select to which stage</label>
                <select name="task-stage-mov-to" id="task-stage-mov-to">
                    <option value="0">BACKLOG</option>
                    <option value="1">DOING</option>
                    <option value="2">QUALITY CHECK</option>
                    <option value="3">DONE</option>
                </select>

                <br><br>
                <input class="submit" type="submit" value="Move">

            </form>
        </div>
    </div>
</div>

</body>
</html>
