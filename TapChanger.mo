within ;
package TapChanger
  model TCULState
    parameter Integer method=1 "Method number";
    parameter Real n=1 "Transformer Ratio";
    parameter Real stepsize=0.01 "Step Size";
    parameter Integer mintap=-12 "Minimum tap step";
    parameter Integer maxtap=12 "Maximum tap step";
    parameter Modelica.SIunits.Duration Tm0=10 "Mechanical Time Delay";
    parameter Modelica.SIunits.Duration Td0=20 "Controller Time Delay 1";
    parameter Modelica.SIunits.Duration Td1=20 "Controller Time Delay 2";
    parameter Modelica.SIunits.PerUnit DB=0.03
      "TCUL Voltage Deadband (double-sided)";
    parameter Modelica.SIunits.PerUnit Vref=1 "TCUL Voltage Reference";
    parameter Modelica.SIunits.PerUnit Vblock=0.82 "Tap locking voltage";
    parameter Boolean InitByVoltage=false "Initialize to V=Vref?";
    inner Real tappos(start=(n - 1)/stepsize) "Current tap step [number]";
    inner Modelica.SIunits.Time offset(start=0);
     Modelica.SIunits.Time Td;
     Modelica.SIunits.Time Tm;
    parameter Real udev=0.1 "Transformer Ratio";

    model Wait

      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{
                100,100}}),
             graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end Wait;
    Wait wait annotation (Placement(transformation(extent={{-10,60},{10,80}})));
    model DownCount
       outer output Modelica.SIunits.Time offset;
       Integer tmp(start=0);
    equation
      tmp = previous(tmp)+1;
      if tmp == 1 then
        /* we need to substract the interval time 
     because of delayed trigger */
        offset = sample(time) - interval(offset);
      else
        offset = previous(offset);
      end if
      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateText",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);

    end DownCount;

    model MechDelay

      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=false);
    end MechDelay;

    model DownAction
       outer output Real tappos;
    equation
        tappos = previous(tappos) - 1;
      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateText",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end DownAction;
    DownAction downAction
      annotation (Placement(transformation(extent={{30,-78},{70,-62}})));
    DownCount downCount
      annotation (Placement(transformation(extent={{22,-10},{78,38}})));
    UpCount upCount
      annotation (Placement(transformation(extent={{-78,2},{-22,38}})));
    UpAction upAction
      annotation (Placement(transformation(extent={{-70,-78},{-30,-62}})));
    model UpCount
      outer output Modelica.SIunits.Time offset;
      Integer tmp(start=0);
    equation
       tmp = previous(tmp) + 1;
       if tmp == 1 then
         offset = sample(time)- interval(offset);
       else
         offset = previous(offset);
       end if
      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateText",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end UpCount;

    model UpAction
       outer output Real tappos;
    equation
        tappos = previous(tappos) + 1;
      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateText",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end UpAction;
    MechDelay downMechDelay
      annotation (Placement(transformation(extent={{42,-38},{58,-22}})));
    MechDelay upMechDelay
      annotation (Placement(transformation(extent={{-58,-38},{-42,-22}})));
  equation
    if method == 1 then
      Td = Td0;
      Tm = Tm0;
    else
      Td = Td0;
      Tm = Tm0;
    end if;

    transition(
      downCount,
      wait,udev <= DB/2,
      priority=1,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{80,14},{94,14},{94,74},{82,76},{70,76},{12,76}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{4,4},{4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
      wait,
      upCount,(udev < -DB/2) and (previous(tappos) + 1 < maxtap),
      priority=2,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{-12,68},{-36,68},{-50,68},{-50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{36,6},{36,12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      upCount,
      wait,udev >= -DB/2,
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{-80,20},{-96,20},{-96,76},{-12,76}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      wait,
      downCount,(udev > DB/2) and (previous(tappos) - 1 > mintap),
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{12,68},{44,68},{50,68},{50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{40,6},{40,12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    initialState(wait) annotation (Line(
        points={{0,82},{0,96},{8,96}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier,
        arrow={Arrow.Filled,Arrow.None}));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics));
    transition(
      downCount,
      downMechDelay,sample(time) - offset > Td,
      immediate=false,
      priority=2,reset=true,synchronize=false)
                  annotation (Line(
        points={{50,-12},{50,-20}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,-4},{-4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      downMechDelay,
      downAction,(sample(time) - offset) > (Td + Tm),immediate=false,reset=true,
      synchronize=false,priority=1)        annotation (Line(
        points={{50,-40},{50,-60}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,-4},{-4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
        downAction,
        wait,
        true,
        immediate=true,
        reset=true,
        synchronize=false,
        priority=1) annotation (Line(
        points={{50,-80},{50,-94},{4,-94},{4,-90},{4,58}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{2,-6},{2,-12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
        upCount,
        upMechDelay,
        (sample(time) - offset) > Td,
        immediate=false,
        priority=2) annotation (Line(
        points={{-50,0},{-50,-20}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,-4},{-4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
        upMechDelay,
        upAction,
        (sample(time) - offset) > (Td + Tm),
        immediate=false,
        reset=true,
        synchronize=false,
        priority=1) annotation (Line(
        points={{-50,-40},{-50,-60}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,-4},{-4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
        upAction,
        wait,
        true) annotation (Line(
        points={{-50,-80},{-50,-96},{-4,-96},{-4,58}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-6,-6},{-6,-12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
  end TCULState;

  model TCULStateTest
  //   parameter Integer method=1 "Method number";
  //   parameter Real n=1 "Transformer Ratio";
  //   parameter Real stepsize=0.01 "Step Size";
  //   parameter Integer mintap=-12 "Minimum tap step";
  //   parameter Integer maxtap=12 "Maximum tap step";
  //   parameter Modelica.SIunits.Duration Tm0=10 "Mechanical Time Delay";
  //   parameter Modelica.SIunits.Duration Td0=20 "Controller Time Delay 1";
  //   parameter Modelica.SIunits.Duration Td1=20 "Controller Time Delay 2";
  //   parameter Modelica.SIunits.PerUnit DB=0.03
  //     "TCUL Voltage Deadband (double-sided)";
  //   parameter Modelica.SIunits.PerUnit Vref=1 "TCUL Voltage Reference";
  //   parameter Modelica.SIunits.PerUnit Vblock=0.82 "Tap locking voltage";
  //   parameter Boolean InitByVoltage=false "Initialize to V=Vref?";
  //   Real tappos(start=(n - 1)/stepsize) "Current tap step [number]";
  //   Modelica.SIunits.Time upcounter(start=-10, fixed=true);
  //   Modelica.SIunits.Time downcounter(start=-10, fixed=true);
  //   Modelica.SIunits.Time Td;
  //   Modelica.SIunits.Time Tm;

    parameter Real udev=0.1 "Transformer Ratio";

    model Wait1

      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end Wait1;
    Wait1 wait1 annotation (Placement(transformation(extent={{-10,60},{10,80}})));
    model CountDown1

      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end CountDown1;

    model ActionDown1

      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end ActionDown1;
    ActionDown1 actionDown1
      annotation (Placement(transformation(extent={{20,-20},{40,0}})));
    CountDown1 countDown1
      annotation (Placement(transformation(extent={{20,20},{40,40}})));
    CountUp1 countUp1
      annotation (Placement(transformation(extent={{-42,20},{-22,40}})));
    ActionUp1 actionUp1
      annotation (Placement(transformation(extent={{-42,-20},{-22,0}})));
    model CountUp1

      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end CountUp1;

    model ActionUp1

      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end ActionUp1;
  equation
  //   if method == 1 then
  //      Td = Td0;
  //      Tm = Tm0;
  //   end if;

    transition(
      countDown1,
      actionDown1,sample(time) > 3,
      immediate=true,
      reset=true,
      synchronize=false,
      priority=2) annotation (Line(
        points={{30,18},{30,2}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      countDown1,
      wait1,sample(time) > 2,
      priority=1,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{42,28},{58,28},{58,76},{46,76},{34,76},{12,76}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{4,4},{4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
      actionDown1,
      wait1,sample(time) > 4,immediate=true,reset=true,synchronize=false,priority=1)
                                    annotation (Line(
        points={{32,-22},{34,-40},{8,-40},{8,58}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{4,-4},{4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
      wait1,
      countUp1,sample(time) > 5,
      priority=2,immediate=true,reset=true,synchronize=false)
                  annotation (Line(
        points={{-12,68},{-32,68},{-32,42}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      countUp1,
      actionUp1,sample(time) > 6,
      priority=2,immediate=true,reset=true,synchronize=false)
                  annotation (Line(
        points={{-32,18},{-32,2}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      countUp1,
      wait1,sample(time) > 7,immediate=true,reset=true,synchronize=false,priority=1)
                                                  annotation (Line(
        points={{-44,28},{-60,28},{-60,76},{-12,76}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      wait1,
      countDown1,sample(time) > 1,immediate=true,reset=true,synchronize=false,priority=1)
                                       annotation (Line(
        points={{12,68},{32,68},{32,42}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      actionUp1,
      wait1,sample(time) > 8,immediate=true,reset=true,synchronize=false,priority=1)
                                  annotation (Line(
        points={{-32,-22},{-32,-34},{-32,-42},{-4,-42},{-4,58}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{4,-4},{4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    initialState(wait1) annotation (Line(
        points={{0,82},{0,96},{8,96}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier,
        arrow={Arrow.Filled,Arrow.None}));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics));
  end TCULStateTest;

  model TCULStateTest2
    parameter Integer method=1 "Method number";
    parameter Real n=1 "Transformer Ratio";
    parameter Real stepsize=0.01 "Step Size";
    parameter Integer mintap=-12 "Minimum tap step";
    parameter Integer maxtap=12 "Maximum tap step";
    parameter Modelica.SIunits.Duration Tm0=10 "Mechanical Time Delay";
    parameter Modelica.SIunits.Duration Td0=20 "Controller Time Delay 1";
    parameter Modelica.SIunits.Duration Td1=20 "Controller Time Delay 2";
    parameter Modelica.SIunits.PerUnit DB=0.03
      "TCUL Voltage Deadband (double-sided)";
    parameter Modelica.SIunits.PerUnit Vref=1 "TCUL Voltage Reference";
    parameter Modelica.SIunits.PerUnit Vblock=0.82 "Tap locking voltage";
    parameter Boolean InitByVoltage=false "Initialize to V=Vref?";
    Real tappos(start=(n - 1)/stepsize) "Current tap step [number]";
    Modelica.SIunits.Time upcounter(start=-10, fixed=true);
    Modelica.SIunits.Time downcounter(start=-10, fixed=true);
    Modelica.SIunits.Time Td;
    Modelica.SIunits.Time Tm;

    parameter Real udev=0.1 "Transformer Ratio";

    Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=DB/2)
      annotation (Placement(transformation(extent={{-40,50},{-20,70}})));
    Modelica.Blocks.Interfaces.RealInput u1 "Connector of Boolean input signal"
      annotation (Placement(transformation(extent={{-140,40},{-100,80}})));
    Modelica.Blocks.Interfaces.RealInput u2 "Connector of Boolean input signal"
      annotation (Placement(transformation(extent={{-140,-80},{-100,-40}})));
    Modelica.Blocks.Math.Feedback feedback
      annotation (Placement(transformation(extent={{-70,50},{-50,70}})));
    Modelica.Blocks.Sources.RealExpression realExpression(y=Vref)
      annotation (Placement(transformation(extent={{-94,30},{-74,50}})));
    Modelica.Blocks.Logical.And and2
      annotation (Placement(transformation(extent={{0,34},{20,54}})));
    Modelica.Blocks.Logical.GreaterThreshold greaterThreshold1(threshold=mintap)
      annotation (Placement(transformation(extent={{-40,20},{-20,40}})));
    Modelica.Blocks.Sources.RealExpression realExpression1(y=tappos)
      annotation (Placement(transformation(extent={{-70,20},{-50,40}})));
    Modelica.Blocks.Logical.Timer timer
      annotation (Placement(transformation(extent={{40,40},{60,60}})));
    Modelica.Blocks.Logical.Not not1
      annotation (Placement(transformation(extent={{0,60},{20,80}})));
    Modelica.Blocks.Logical.LogicalSwitch logicalSwitch
      annotation (Placement(transformation(extent={{-14,-28},{6,-8}})));
    Modelica.Blocks.Logical.GreaterThreshold greaterThreshold2(threshold=Td +
          downcounter)
      annotation (Placement(transformation(extent={{80,40},{100,60}})));
    Modelica.Blocks.Logical.Switch switch1
      annotation (Placement(transformation(extent={{120,-10},{140,10}})));
    Modelica.Blocks.Interfaces.RealOutput y "Connector of Real output signal"
      annotation (Placement(transformation(extent={{160,-10},{180,10}})));
  equation
    connect(u1, feedback.u1) annotation (Line(
        points={{-120,60},{-68,60}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(realExpression.y, feedback.u2) annotation (Line(
        points={{-73,40},{-60,40},{-60,52}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(greaterThreshold.u, feedback.y) annotation (Line(
        points={{-42,60},{-51,60}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(and2.u1, greaterThreshold.y) annotation (Line(
        points={{-2,44},{-12,44},{-12,60},{-19,60}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(greaterThreshold1.y, and2.u2) annotation (Line(
        points={{-19,30},{-14,30},{-14,36},{-2,36}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(realExpression1.y, greaterThreshold1.u) annotation (Line(
        points={{-49,30},{-42,30}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(and2.y, timer.u) annotation (Line(
        points={{21,44},{32,44},{32,50},{38,50}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(not1.u, greaterThreshold.y) annotation (Line(
        points={{-2,70},{-12,70},{-12,60},{-19,60}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(timer.y, greaterThreshold2.u) annotation (Line(
        points={{61,50},{78,50}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(switch1.y, y) annotation (Line(
        points={{141,0},{170,0}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(greaterThreshold2.y, switch1.u2) annotation (Line(
        points={{101,50},{102,50},{102,50},{100,50},{110,50},{110,0},{118,0}},
        color={255,0,255},
        smooth=Smooth.None));

  //   if method == 1 then
  //      Td = Td0;
  //      Tm = Tm0;
  //   end if;

    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{160,100}}), graphics), Icon(coordinateSystem(extent={{-100,
              -100},{160,100}})));
  end TCULStateTest2;

  model Test

    inner Integer i( start=0);
    model State1
      outer output Integer i;
    equation

      i = previous(i)+2;
      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end State1;
    State1 state1
      annotation (Placement(transformation(extent={{-62,42},{-42,62}})));
    model State2
     outer output Integer i;
    equation

      i = previous(i)-1;
      annotation (
        Icon(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%name")}),
        Diagram(graphics={Text(
              extent={{-100,100},{100,-100}},
              lineColor={0,0,0},
              textString="%stateName",
              fontSize=10)}),
        __Dymola_state=true,
        showDiagram=true,
        singleInstance=true);
    end State2;
    State2 state2
      annotation (Placement(transformation(extent={{-60,-8},{-40,12}})));
  equation
    transition(
      state1,
      state2,i > 10,
      immediate=false,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{-50,40},{-50,14}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,-4},{-4,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      state2,
      state1,i < 1,immediate=false,reset=true,synchronize=false,priority=1)
                annotation (Line(
        points={{-50,-10},{-14,-46},{14,34},{-40,52},{-40,54}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-4,4},{-4,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    initialState(state1) annotation (Line(
        points={{-48,64},{-52,82}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier,
        arrow={Arrow.Filled,Arrow.None}));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics));
  end Test;
  annotation (uses(Modelica(version="3.2.1")));
end TapChanger;
