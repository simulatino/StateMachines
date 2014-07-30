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
    Boolean blocked=sample(u2) < Vblock "Tap locked ?";

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
    Wait wait annotation (Placement(transformation(extent={{-10,44},{10,64}})));
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
      annotation (Placement(transformation(extent={{28,-78},{72,-62}})));
    DownCount downCount
      annotation (Placement(transformation(extent={{18,-14},{82,38}})));
    UpCount upCount
      annotation (Placement(transformation(extent={{-82,2},{-18,38}})));
    UpAction upAction
      annotation (Placement(transformation(extent={{-72,-78},{-28,-62}})));
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
      annotation (Placement(transformation(extent={{42,-44},{58,-28}})));
    Delay upMechDelay
      annotation (Placement(transformation(extent={{-58,-38},{-42,-22}})));
    Delay delay annotation (Placement(transformation(extent={{-6,78},{6,88}})));
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
    tappos = (y - 1)/stepsize;
    y1 = tappos_offset;
    y2 = 1*time "Currently y2 is not in use and set to zero";
    transition(
      downCount,
      wait,sample(Vdev) <= DB/2,
      priority=1,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{84,12},{94,14},{94,52},{94,60},{70,60},{12,60}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{8,0},{8,6}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
      wait,
      upCount,(sample(Vdev) < -DB/2) and (previous(tappos) + 1 < maxtap) and not
      blocked,
      priority=2,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{-12,52},{-36,52},{-50,52},{-50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{36,4},{36,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      upCount,
      wait,(sample(Vdev) >= -DB/2) or blocked,
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{-84,20},{-96,20},{-96,60},{-12,60}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{-8,0},{-8,6}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      wait,
      downCount,(sample(Vdev) > DB/2) and (previous(tappos) - 1 > mintap),
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{12,52},{44,52},{50,52},{50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{58,4},{58,10}},
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
        points={{50,-16},{50,-26}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{18,-6},{18,-12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      downMechDelay,
      downAction,(sample(time) - offset) > (Td + Tm),immediate=false,reset=true,
      synchronize=false,priority=1)        annotation (Line(
        points={{50,-46},{50,-60}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{20,-6},{20,-12}},
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
        points={{50,-80},{50,-94},{28,-94},{4,-94},{4,42}},
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
        extent={{18,-8},{18,-14}},
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
        extent={{32,-10},{32,-16}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      upAction,
      wait,
      true) annotation (Line(
        points={{-50,-80},{-50,-96},{-4,-96},{-4,42}},
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
        points={{0,76},{0,66}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{52,4},{52,10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    initialState(delay) annotation (Line(
        points={{0,90},{0,96},{4,96},{10,96}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier,
        arrow={Arrow.Filled,Arrow.None}));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}), graphics));
  end TCULState;

  model Test
    TCULState tCULState
      annotation (Placement(transformation(extent={{-2,-2},{18,18}})));
    Modelica.Blocks.Sources.Sine Sine(
      amplitude=0.05,
      freqHz=1/100,
      offset=1)
      annotation (Placement(transformation(extent={{-62,10},{-42,30}})));
    Modelica.Blocks.MathInteger.Product
                        product(nu=2)
      annotation (Placement(transformation(extent={{48,2},{60,14}})));
    Modelica.Blocks.Interaction.Show.IntegerValue showValue1
      annotation (Placement(transformation(extent={{74,-2},{94,18}})));
  equation
    connect(Sine.y, tCULState.u1) annotation (Line(
        points={{-41,20},{-16,20},{-16,14},{-4,14}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(Sine.y, tCULState.u2) annotation (Line(
        points={{-41,20},{-24,20},{-24,2},{-4,2}},
        color={0,0,127},
        smooth=Smooth.None));
    connect(product.y,showValue1. numberPort) annotation (Line(
        points={{60.9,8},{72.5,8}},
        color={255,127,0},
        smooth=Smooth.None));
    connect(tCULState.y1, product.u[1]) annotation (Line(
        points={{19,14},{38.5,14},{38.5,10.1},{48,10.1}},
        color={255,127,0},
        smooth=Smooth.None));
    connect(tCULState.y2, product.u[2]) annotation (Line(
        points={{19,2},{34,2},{34,5.9},{48,5.9}},
        color={255,127,0},
        smooth=Smooth.None));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{
              -100,-100},{100,100}}), graphics));
  end Test;
  annotation (uses(Modelica(version="3.2.1")));
end TapChanger;
