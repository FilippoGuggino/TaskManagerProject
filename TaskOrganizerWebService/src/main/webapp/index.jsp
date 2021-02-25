<%@ page import="java.util.ArrayList" %><%--
  Created by IntelliJ IDEA.
  User: Leonardo
  Date: 09/02/2021
  Time: 23:31
  To change this template use File | Settings | File Templates.
--%>
<%@ page contentType="text/html;charset=UTF-8" language="java" %>
<html>
<head>
    <title>Task Organizer Welcome Page</title>

</head>
<body>

<%  ArrayList<String> board_list = (ArrayList<String>) request.getAttribute("board_list");
%>

<h1>Task Organizer Welcome Page</h1>

<p>Select a board from the menu list</p>

<form action="Board">
    <label for="selectBoard">Choose a board</label>
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
    <input type="submit" value="Submit">
</form>

</body>
</html>
