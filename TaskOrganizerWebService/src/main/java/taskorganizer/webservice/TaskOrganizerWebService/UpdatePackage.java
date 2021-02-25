package taskorganizer.webservice.TaskOrganizerWebService;

import java.util.ArrayList;

public class UpdatePackage {
    String boardname;
    ArrayList<Task> backlog_tasks;
    ArrayList<Task> doing_tasks;
    ArrayList<Task> quality_check_tasks;
    ArrayList<Task> done_tasks;

    public UpdatePackage(String boardname, ArrayList<Task> backlog_tasks, ArrayList<Task> doing_tasks,
                         ArrayList<Task> quality_check_tasks, ArrayList<Task> done_tasks){
        this.boardname = boardname;
        this.backlog_tasks = backlog_tasks;
        this.doing_tasks = doing_tasks;
        this.quality_check_tasks = quality_check_tasks;
        this.done_tasks = done_tasks;
    }

    public String getBoardname(){
        return this.boardname;
    }

    public ArrayList<Task>getBacklog(){
        return this.backlog_tasks;
    }

    public ArrayList<Task>getDoing(){
        return this.doing_tasks;
    }

    public ArrayList<Task>getQualityCheck(){
        return this.quality_check_tasks;
    }

    public ArrayList<Task>getDone(){
        return this.done_tasks;
    }
}
