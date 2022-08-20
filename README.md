# FinalPorjectFPinCandDS
Final Project in Functional programming in concurrent and distributed systems. Simulation of the blood cells function inside our body.

In order to run the program first there is need in the ip of all the computer that will be running this program (max number of five).
After getting all the ip adress select one for the main process (the head), and a process ip for every limb (4 in total).
Open a folder and switch the Define addresses with the name of your node icluding the ip, in the all the places where the defines, 
accourding to you selection when "My_node" is the adress of the corrent process, "Head_Node" is the main process and for each imb there is 
its nieghbor limb: hand0 -> hand1 -> foot0 -> foot1 -> hand0. so in the define "Next_node" write the ip address of the corresonding node as
decided.
The files which need to be addited are foot0, foot1, hand0, hand1, all recover* files in each limb folder and in head in head folder.
After that in each computer open the corresponding folder of the limb and run erlang using: "erl -name *selected corresponding name and ip* -setcookie cook
compile the file named: main_*limbname* and run main_*limbnmae*:start().

