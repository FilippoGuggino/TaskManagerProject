# DistributedSystem - TaskOrganizer

## Description of the system 

<p>TaskOrganizer is a web application whose goal is to deliver a task manager system for projects.<\p>
<p>The main goals of this application are:</p> 
<ul>
  <li> Provide a simple interface to create boards and tasks <\li>
  <li> Update in real time the boards of every user connected to the system <\li>
  <li> fault-tollerance of the data by replication of the back-end <\li>
</ul>

## Architecture overview
![picture](https://github.com/FilippoGuggino/TaskManagerProject/blob/master/Doc/Architecture.png)

<p>The full documentation is available here: (https://github.com/FilippoGuggino/TaskManagerProject/blob/master/Doc/Task_Organizer_Doc.pdf) <\p>

## Installation Guide

<p>In order to install containers, compile and deploy the project(execute inside TaskManagerProject folder):</p>

```
sudo make build
sudo make deploy
```
<p>This can take ~10 min (it may be required to press Enter and digit 'exit' a few times when the process stops)</p>

<p>Service can be accessed at the link: </p>

```
http://172.18.0.150/TaskOrganizerWebService-1.0-SNAPSHOT/welcome
```

## Authors
 - Riccardo Xefraj
 - Filippo Guggino
 - Leonardo Cecchelli

