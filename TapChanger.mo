within ;
package TapChanger
  model TCULState
    extends Modelica.Blocks.Interfaces.SI2SO;
    /* Time parameters */
    parameter Modelica.SIunits.Time ActivationTime=3
      "Time from which on the TCUL controller shall be activated (default=0)"
      annotation (Dialog(group="Time settings"));
    parameter Modelica.SIunits.Duration Tm0=10 "Mechanical Time Delay"
      annotation (Dialog(group="Time settings"));
    parameter Modelica.SIunits.Duration Td0=20 "Controller Time Delay 1"
      annotation (Dialog(group="Time settings"));
    parameter Modelica.SIunits.Duration Td1=20 "Controller Time Delay 2"
      annotation (Dialog(group="Time settings"));

    /* Tap parameters */
    parameter Real n=1 "Initial transformer ratio"
        annotation (Dialog(group="Tap settings"));
    parameter Integer mintap=-16 "Minimum tap step"
        annotation (Dialog(group="Tap settings"));
    parameter Integer maxtap=16 "Maximum tap step"
        annotation (Dialog(group="Tap settings"));
    parameter Modelica.SIunits.PerUnit DB=0.02
      "TCUL Voltage Deadband (double-sided)"
        annotation (Dialog(group="Tap settings"));
    parameter Real stepsize=DB/(abs(maxtap)+abs(mintap)) "Step size"
        annotation (Dialog(group="Tap settings"));
    parameter Integer method(min=1, max=4)=1
      "Method number (mechanical characteristic)"
        annotation (Dialog(group="Tap settings"));

    /* Voltage parameters */
    parameter Modelica.SIunits.PerUnit Vref=1 "TCUL Voltage Reference"
        annotation (Dialog(group="Voltage settings"));
    parameter Modelica.SIunits.PerUnit Vblock=0.82 "Tap locking voltage"
        annotation (Dialog(group="Voltage settings"));

    /* Sampling settings */
    parameter Modelica.SIunits.Time SamplePeriod=1
      "Sample period for the input signals"
      annotation (Dialog(tab="Sampling settings"));
    Clock clk = Clock(SamplePeriod) "Definition of a global Clock";
    inner Modelica.SIunits.Time time_s = sample(time, clk) "Sampled time";
    Modelica.SIunits.PerUnit u1_s = sample(u1, clk) "Sampled input signal u1";
    Modelica.SIunits.PerUnit u2_s = sample(u2, clk) "Sampled input signal u1";

    /* Other variables */
    inner Real tappos(start=(n - 1)/stepsize) "Current tap step [number]";
    Integer tappos_offset(start=abs(mintap)) = integer(tappos+0.5) + abs(mintap)
      "Tap step shifted by mintap";
    inner Modelica.SIunits.Time offset(start=0)
      "Temp variable used in the states";
    Modelica.SIunits.Time Td "Delay time before tapping is triggered";
    Modelica.SIunits.Time Tm "Delay time for the mechanical tapping";
    Modelica.SIunits.PerUnit Vdev "Voltage deviation";
    Boolean blocked=u2_s < Vblock "Tap locked ?";

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
       outer Modelica.SIunits.Time time_s;
       Integer tmp(start=0);
    equation
      tmp = previous(tmp)+1;
      if tmp == 1 then
        /* we need to substract the interval time 
     because of delayed trigger */
        offset = time_s - interval(offset);
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
      annotation (Placement(transformation(extent={{28,-84},{72,-68}})));
    DownCount downCount
      annotation (Placement(transformation(extent={{14,-24},{86,38}})));
    UpCount upCount
      annotation (Placement(transformation(extent={{-86,-16},{-14,38}})));
    UpAction upAction
      annotation (Placement(transformation(extent={{-72,-78},{-28,-62}})));
    model UpCount
      outer output Modelica.SIunits.Time offset;
      outer Modelica.SIunits.Time time_s;
      Integer tmp(start=0);
    equation
       tmp = previous(tmp) + 1;
       if tmp == 1 then
         offset = time_s- interval(offset);
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
      annotation (Placement(transformation(extent={{42,-56},{58,-40}})));
    Delay upMechDelay
      annotation (Placement(transformation(extent={{-58,-46},{-42,-30}})));
    Delay delay annotation (Placement(transformation(extent={{-6,78},{6,88}})));
    Modelica.Blocks.Interfaces.IntegerOutput y1
      annotation (Placement(transformation(extent={{100,50},{120,70}})));

  equation
    if (method == 1) then
      Td = Td0;
      Tm = Tm0;
    elseif (method == 2) then
      Td = Td0*DB/2/max(abs(Vdev), 1e-6);
      Tm = Tm0;
    elseif (method == 3) then
      Td = Td0*DB/2/max(abs(Vdev), 1e-6);
      Tm = Tm0*DB/2/max(abs(Vdev), 1e-6);
    else
      Td = Td1 + Td0*DB/2/max(abs(Vdev), 1e-6);
      Tm = Tm0;
    end if;

    Vdev = u1_s - Vref;
    hold(tappos) = (y - 1)/stepsize; // hold the discrete signals for output
    /* Hold and delay the discrete signals for output.
  Not quite clear why the delay but this is required
  for coupling with the EPL/Spot type transformer models. */
    y1 = hold(previous(tappos_offset));

    transition(
      downCount,
      wait,Vdev <= DB/2,
      priority=1,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{88,7},{98,7},{98,52},{98,60},{70,60},{12,60}},
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
      upCount,(Vdev < -DB/2) and (previous(tappos) < maxtap) and not blocked,
      priority=2,
      immediate=true,
      reset=true,
      synchronize=false) annotation (Line(
        points={{-12,52},{-36,52},{-50,52},{-50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{32,6},{32,12}},
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
        points={{-88,11},{-98,11},{-98,60},{-12,60}},
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
      downCount,(Vdev > DB/2) and (previous(tappos) > mintap),
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{12,52},{44,52},{50,52},{50,40}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{48,6},{48,12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));

    transition(
      downCount,
      downMechDelay,time_s - offset > Td,
      immediate=false,
      priority=2,reset=true,synchronize=false)
                  annotation (Line(
        points={{50,-26},{50,-38}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{8,-4},{8,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      downMechDelay,
      downAction,(time_s - offset) > (Td + Tm),immediate=false,reset=true,
      synchronize=false,priority=1)        annotation (Line(
        points={{50,-58},{50,-66}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{22,-2},{22,-8}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      downAction,
      wait,true,
      immediate=true,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{50,-86},{50,-94},{28,-94},{4,-94},{4,42}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{0,-6},{0,-12}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Left));
    transition(
      upCount,
      upMechDelay,
      (time_s - offset) > Td,
      immediate=false,
      priority=2) annotation (Line(
        points={{-50,-18},{-50,-28}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{22,-2},{22,-8}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      upMechDelay,
      upAction,
      (time_s - offset) > (Td + Tm),
      immediate=false,
      reset=true,
      synchronize=false,
      priority=1) annotation (Line(
        points={{-50,-48},{-50,-60}},
        color={175,175,175},
        thickness=0.25,
        smooth=Smooth.Bezier), Text(
        string="%condition",
        extent={{22,-4},{22,-10}},
        lineColor={95,95,95},
        fontSize=10,
        textStyle={TextStyle.Bold},
        horizontalAlignment=TextAlignment.Right));
    transition(
      upAction,
      wait,true,immediate=true, reset=true,synchronize=false,priority=1)
            annotation (Line(
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
      wait,time_s >= ActivationTime,immediate=true,reset=true,synchronize=false,
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
              -100},{100,100}}), graphics), Documentation(info="Discrete implementation of Tap-Changing Under Load (TCUL) control system
according to method D1-D4 in [1] using a state-machine implementation of
the control system and tap changer mechanism.

Ideally the variable 'tappos' should be initialized to give a voltage
deviation within the deadband at the start of the simulatation. 
Failure to do so may result in convergence problems with the initial 
value solver.

Using the parameter the characteristics of the mechanical delay time (Tm)
and the controlled delay time (Td) the TCUL can be influenced according
to the table below:

----------------------------------
|method|    Td     |    Tm       |
----------------------------------
|  1   |  constant |  constant   |
|  2   |  inverse  |  constant   |
|  3   |  inverse  |  inverse    |
|  4   |  both     |  constant   |
----------------------------------

The tap changer is locked if the primary side voltage decreases below the
tap locking voltage specified in Vblock.
---
[1] P.W. Sauer and M.A. Pai, \"A comparison of discrete vs. continuous
dynamic models of tap-changing-under-load transformers\", in Proceedings
of NSF/ECC Workshop on Bulk power System Voltage Phenomena - III :
Voltage Stability, Security and Control,  Davos, Switzerland, 1994.
"));
  end TCULState;

  model Test

    TCULState tCULState
      annotation (Placement(transformation(extent={{-2,-2},{18,18}})));
    Modelica.Blocks.Sources.Ramp Ramp(
      height=-0.2,
      offset=1.1,
      duration=500)
      annotation (Placement(transformation(extent={{-62,10},{-42,30}})));
    Modelica.Blocks.MathInteger.Product
                        product(nu=2)
      annotation (Placement(transformation(extent={{48,2},{60,14}})));
    Modelica.Blocks.Interaction.Show.IntegerValue showValue1
      annotation (Placement(transformation(extent={{74,-2},{94,18}})));
    Modelica.Blocks.Sources.IntegerStep integerStep(offset=2, startTime=2)
      annotation (Placement(transformation(extent={{0,-36},{20,-16}})));
    Modelica.Blocks.Sources.Sine Sine1(
      amplitude=0.05,
      offset=1,
      freqHz=1/50)
      annotation (Placement(transformation(extent={{-60,-30},{-40,-10}})));
  equation
    connect(Ramp.y, tCULState.u1) annotation (Line(
        points={{-41,20},{-16,20},{-16,14},{-4,14}},
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
    connect(integerStep.y, product.u[2]) annotation (Line(
        points={{21,-26},{34,-26},{34,5.9},{48,5.9}},
        color={255,127,0},
        smooth=Smooth.None));
    connect(Sine1.y, tCULState.u2) annotation (Line(
        points={{-39,-20},{-20,-20},{-20,2},{-4,2}},
        color={0,0,127},
        smooth=Smooth.None));
    annotation (Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,
              -100},{100,100}}),      graphics));
  end Test;
  annotation (uses(Modelica(version="3.2.1")));
end TapChanger;
