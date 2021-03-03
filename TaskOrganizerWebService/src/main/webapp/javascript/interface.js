/**
 * This function adds all task html elements to the backlog-stage div.
 *
 * @param title Title of the task
 * @param type Type of the the task
 * @param description Description of the task
 * @param expiration Expiration date of the task
 * @param creator Creator of the task
 */

function addTask(title, type, description, expiration, creator){

    var currentDiv = document.getElementById("backlog-stage");
    var newTaskDiv = document.createElement("div")

    newTaskDiv.className = "task_div";
    var today = new Date();
    var compare_date = new Date(expiration);
    if (compare_date >= today){
        newTaskDiv.style.color = "green";
    }
    else {
        newTaskDiv.style.color = "red";
    }

    //title
    var newTaskTitle = document.createElement("h1");
    newTaskTitle.className = "title";
    var TaskTitle = document.createTextNode(title);
    newTaskTitle.appendChild(TaskTitle);
    newTaskDiv.appendChild(newTaskTitle);

    //type
    var newTaskType = document.createElement("h2");
    newTaskType.className = "type";
    var TaskType = document.createTextNode(type);
    newTaskType.appendChild(TaskType);
    newTaskDiv.appendChild(newTaskType);

    //description
    var newTaskDesc = document.createElement("p");
    newTaskDesc.className = "desc";
    var TaskDesc = document.createTextNode(description);
    newTaskDesc.appendChild(TaskDesc);
    newTaskDiv.appendChild(newTaskDesc);

    //creator
    var newTaskCreator = document.createElement("p");
    newTaskCreator.className = "created";
    var TaskCreator = document.createTextNode("Created by: "+creator);
    newTaskCreator.appendChild(TaskCreator);
    newTaskDiv.appendChild(newTaskCreator);

    //expiration
    var newTaskExp = document.createElement("p");
    newTaskExp.className = "expiration";
    var TaskExp = document.createTextNode("Expires on: "+expiration);
    newTaskExp.appendChild(TaskExp);

    newTaskDiv.appendChild(newTaskExp);
    currentDiv.appendChild(newTaskDiv);
}

/**
 * This function deletes all the html elements of a certain task from the selected stage div.
 *
 * @param title Title of the task
 * @param stage Stage number from which the task has to be removed
 * @returns True if the task has been found and deleted, False if the task was not found.
 */



function deleteTask(title,stage){
    var tasks;

    if (stage == 0){
        tasks = document.getElementById('backlog-stage').children;
    }
    if (stage == 1){
        tasks = document.getElementById('doing-stage').children;
    }
    if (stage == 2){
        tasks = document.getElementById('quality-check-stage').children;
    }
    if (stage == 3){
        tasks = document.getElementById('done-stage').children;
    }
    for (let i = 1; i < tasks.length; i++){
        var task = tasks[i].children;
        if (task[0].textContent == title){

            var task_to_delete =  tasks[i];
            if(stage == 0) {
                document.getElementById('backlog-stage').removeChild(task_to_delete);
            }
            if(stage == 1) {
                document.getElementById('doing-stage').removeChild(task_to_delete);
            }
            if(stage == 2) {
                document.getElementById('quality-check-stage').removeChild(task_to_delete);
            }
            if(stage == 3) {
                document.getElementById('done-stage').removeChild(task_to_delete);
            }
            return true;
        }
    }
    return false;
}

/**
 * This function clones all the html elements of a certain task, deletes the corresponding node
 * from the from_stage div and add the cloned one to the to_stage div.
 *
 * @param title Title of the task
 * @param from_stage Stage number from which the task has to be removed
 * @param to_stage Stage number to which the task has to be added
 * @returns True if the task has been found and moved, False if the task was not found.
 */

function moveTask(title, from_stage, to_stage){

    var task_cln;
    var tasks;
    var currentStage;

    //select from_stage div

    if (from_stage == 0){
        tasks = document.getElementById('backlog-stage').children;
    }
    if (from_stage == 1){
        tasks = document.getElementById('doing-stage').children;
    }
    if (from_stage == 2){
        tasks = document.getElementById('quality-check-stage').children;
    }
    if (from_stage == 3){
        tasks = document.getElementById('done-stage').children;
    }

    //select to_stage div

    if (to_stage == 0){
        currentStage = document.getElementById("backlog-stage");
    }
    if (to_stage == 1){
        currentStage = document.getElementById("doing-stage");
    }
    if (to_stage == 2){
        currentStage = document.getElementById("quality-check-stage");
    }
    if (to_stage == 3){
        currentStage = document.getElementById("done-stage");
    }
    for (let i = 1; i < tasks.length; i++){
        var task = tasks[i].children;
        if (task[0].textContent == title){

            var task_to_delete =  tasks[i];
            task_cln = task_to_delete.cloneNode(true);

            if(from_stage == 0) {
                document.getElementById('backlog-stage').removeChild(task_to_delete);
            }
            if(from_stage == 1) {
                document.getElementById('doing-stage').removeChild(task_to_delete);
            }
            if(from_stage == 2) {
                document.getElementById('quality-check-stage').removeChild(task_to_delete);
            }
            if(from_stage == 3) {
                document.getElementById('done-stage').removeChild(task_to_delete);
            }
            currentStage.appendChild(task_cln);
            return true;
        }
    }
    return false;
}


//TESTING
//do not use this functions, only for testing purposes!

function addTask_test(){
    const name = document.getElementById('task-name').value;
    const type = document.getElementById('task-type').value;
    const desc = document.getElementById('task-desc').value;
    const exp = document.getElementById('task-exp').value;
    const cre = document.getElementById('task-creator').value;
    addTask(name,type,desc,exp,cre);
}

function moveTask_test(){
    const name = document.getElementById('task-name-move').value;
    const from = document.getElementById('task-stage-mov-from').value;
    const to = document.getElementById('task-stage-mov-to').value;

    moveTask(name,from,to);
}

function  deleteTask_test(){
    const name = document.getElementById('task-name-del').value;
    const from = document.getElementById('task-stage-del').value;

    deleteTask(name,from);
}
