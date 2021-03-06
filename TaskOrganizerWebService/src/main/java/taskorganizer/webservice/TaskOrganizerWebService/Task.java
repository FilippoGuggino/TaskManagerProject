package taskorganizer.webservice.TaskOrganizerWebService;

import java.text.SimpleDateFormat;
import java.util.Date;

public class Task {
    int taskID;
    String title;
    int stage_index;
    String type;
    String description;
    Date expiration;
    String creator;
    boolean deleted;
    int stage_integral_number;

    public Task(String title, String type, String description, Date expiration, String creator) {

        this.title = title;
        this.stage_index = 0;
        this.type = type;
        this.description = description;
        this.expiration = expiration;
        this.creator = creator;
        this.deleted = false;

    }

    //some utility functions
    public int isID() { return this.taskID; }
    public String isTitle(){
        return this.title;
    }
    public String isCreator(){
        return this.creator;
    }
    public String isDescription(){
        return this.description;
    }
    public int currentStage(){
        return this.stage_index;
    }
    public String isType(){
        return this.type;
    }
    public boolean isDeleted() {
        return this.deleted;
    }

    public boolean isExpired() {
        Date actualDate = new Date();
        if (actualDate.after(this.expiration)){
            return true;
        }
        return false;
    }
    public String expirationDate(){
        SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
        String formattedDate = formatter.format(this.expiration);
        return formattedDate;
    }

    public void setID(int id){
        this.taskID = id;
    }

    public void setIntegralStage(int stage){
        this.stage_integral_number = stage;
    }

    public void setStage_index(int stage_index) {
        this.stage_index = stage_index;
    }

    public int computeNewStage(int toStage){
        int diff = toStage - stage_index;
        return stage_integral_number + diff;
    }
}
