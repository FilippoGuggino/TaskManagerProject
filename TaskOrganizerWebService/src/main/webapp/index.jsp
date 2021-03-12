<%@ page import="java.util.ArrayList" %>
<%@ page import="taskorganizer.webservice.TaskOrganizerWebService.MessageManager" %><%--
  Created by IntelliJ IDEA.
  User: Leonardo
  Date: 09/02/2021
  Time: 23:31
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<link rel="stylesheet" href="css/welcome.css">
<head>
    <title>Task Organizer Welcome Page</title>

</head>
<body>
<% out.print("<p>Connected to " + MessageManager.ip + "</p>"); %>

<%  ArrayList<String> board_list = (ArrayList<String>) request.getAttribute("board_list");
%>

<h1 id="title_1">Task</h1>
<h1 id="title_2"> Organizer</h1>

<div id="selector_div">
    <p>Start organizing your workflow!</p>
    <form action="Board" id="selector">
        <label for="selectBoard">Choose a board: </label>
        <select name="selectBoard" id="selectBoard">
            <%
                if (!board_list.isEmpty()) {
                    for (int i = 0; i < board_list.size(); i++) {
                        out.println("<option value=" + board_list.get(i) + ">" + board_list.get(i) + "</option>");
                    }
                }
            %>
        </select>
        <br><br>
        <input id ="submit_button" type="submit" value="Submit">
    </form>

    <form action="createBoard" id="newBoard">
        <p>Create a new board</p>

        <label for="board-name">Board name</label>
        <input type="text" placeholder="Enter new board name" name="board-name" id="board-name" required>

        <br><br>
        <input class="submit" type="submit" value="Create">
    </form>

</div>

</body>
</html>
