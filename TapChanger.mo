within ;
package TapChanger
  model TCULState
    extends Modelica.Blocks.Interfaces.SI2SO;
    parameter Modelica.SIunits.Time ActivationTime=3
      "Time from which on the TCUL controller shall be activated (default=0)";
    parameter Integer method=1 "Method number (mechanical characteristic)";
    parameter Real n=1 "Initial transformer Ratio";
    parameter Real stepsize=DB/(abs(maxtap)+abs(mintap)) "Step size";
    parameter Integer mintap=-16 "Minimum tap step";
    parameter Integer maxtap=16 "Maximum tap step";
    parameter Modelica.SIunits.Duration Tm0=10 "Mechanical Time Delay";
    parameter Modelica.SIunits.Duration Td0=20 "Controller Time Delay 1";
    parameter Modelica.SIunits.Duration Td1=20 "Controller Time Delay 2";
    parameter Modelica.SIunits.PerUnit DB=0.02
      "TCUL Voltage Deadband (double-sided)";
    parameter Modelica.SIunits.PerUnit Vref=1 "TCUL Voltage Reference";
    parameter Modelica.SIunits.PerUnit Vblock=0.82 "Tap locking voltage";
    parameter Boolean InitByVoltage=false "Initialize to V=Vref?";
    inner Real tappos(start=(n - 1)/stepsize) "Current tap step [number]";
    Integer tappos_offset = integer(tappos+0.5) + abs(mintap)
      "Tap step shifted by mintap";
    inner Modelica.SIunits.Time offset(start=0)
      "Temp variable used in the states";
    Modelica.SIunits.Time Td "Delay time before tapping is triggered";
    Modelica.SIunits.Time Tm "Delay time for the mechanical tapping";
    Modelica.SIunits.PerUnit Vdev "Voltage deviation";
    Boolean blocked=u2 < Vblock "Tap locked ?";

    model Wait

      annotation (
        Icon(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},{100,
                100}}),
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
    Wait wait annotation (Placement(transformation(extent={{-10,42},{10,62}})));
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

    model Delay

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
    end Delay;

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
    Delay downMechDelay
      annotation (Placement(transformation(extent={{42,-38},{58,-22}})));
    Delay upMechDelay
      annotation (Placement(transformation(extent={{-58,-38},{-42,-22}})));
    Delay delay annotation (Placement(transformation(extent={{-6,74},{6,84}})));
    Modelica.Blocks.Interfaces.IntegerOutput y1
      annotation (Placement(transformation(extent={{100,50},{120,70}})));
    Modelica.Blocks.Interfaces.IntegerOutput y2
      annotation (Placement(transformation(extent={{100,-70},{120,-50}})));
  equation
    if method == 1 then
      Td = Td0;
      Tm = Tm0;
    else
      Td = Td0;
      Tm = Tm0;
    end if;
    Vdev = u1 - Vref;
    tappos = (y - 1)/stepsize + 0.5;  // convert to nearest integer
    y1 = tappos_offset;
    y2 = 0 "Currently y2 is not in use and set to zero";
    transition(
      downCount,
      wait,Vdev <= DB/2,
      priority=1,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{80,14},{94,14},{94,52},{94,58},{70,58},{12,58}},
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
      upCount,(Vdev < -DB/2) and (previous(tappos) + 1 < maxtap) and not blocked,
      priority=2,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{-12,50},{-50,50},{-50,40}},
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
      wait,(Vdev >= -DB/2) or blocked,
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{-80,20},{-96,20},{-96,58},{-12,58}},
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
      downCount,(Vdev > DB/2) and (previous(tappos) - 1 > mintap),
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{12,50},{44,50},{50,50},{50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{40,6},{40,12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));

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
        points={{50,-80},{50,-94},{4,-94},{4,-90},{4,40}},
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
        points={{-50,-80},{-50,-96},{-4,-96},{-4,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-6,-6},{-6,-12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
      delay,
      wait,sample(time) >= ActivationTime,immediate=true,reset=true,synchronize=false,
      priority=1) annotation (Line(
        points={{0,72},{0,64}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{8,2},{8,8}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    initialState(delay) annotation (Line(
        points={{0,86},{0,96},{2,96},{6,96}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier,
        arrow={Arrow.Filled,Arrow.None}));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics));
  end TCULState;


  annotation (uses(Modelica(version="3.2.1")));
end TapChanger;
