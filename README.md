# FM 2018

The objective of the following project is to describe a formal model of the interaction of an Human Operator and a Robot.
The Robot to be modelled is analogous to the [KUKA KMR-iiwa](https://www.kuka.com/en-us/products/robotics-systems) which is a manipulator and mobile robot, used mainly for industrial use: welding, assembly, loading and unloading.

The scenario is one of Robot Human Collaboration where the Robot has to aid the operator in performing a given task.
Specifically the Robot has to bring Work Pieces from a Global Bin to a Work Station.

After being elaborated in some fashion by either additional machinery or the Operator, the pieces are placed on a Conveyor Belt.

Normally the Robot operates in a sort of loop, going back and forth from the Global Bin to the Work Station in order to bring pieces.

It's also possible for the Operator to give the Robot additional commands.

For a more precise description of the Robot tasks it's possible to refer to the graph below.

![](https://github.com/LorenzoNorcini/FM_2018/blob/master/MainLoop.png)

The robot enviroment is formalized as follows.

![](https://github.com/LorenzoNorcini/FM_2018/blob/master/Map.png)

The formal description of the system is done using [TRIO](http://risorse.dei.polimi.it/TRIO/) formal language and can be found [here](https://github.com/LorenzoNorcini/FM_2018/blob/master/Formal%20Methods.pdf)

A simplified version of the model has been verified using [Zot model checker](https://arxiv.org/abs/0912.5014)

In order to run the script, Docker must be installed
```bash
./run.sh zot FM.lisp
```
