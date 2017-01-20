package sparkPkg16 "Spark package year 2016 - with zener clamp"
  package C_based
    model Cspark0 "Solo carica induttore"
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(visible=true, transformation(
            origin={-64,-16},
            extent={{-10,-10},{10,10}},
            rotation=-90)));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.5) annotation (
          Placement(visible=true, transformation(
            origin={-64,16},
            extent={{-10,-10},{10,10}},
            rotation=90)));
      Modelica.Electrical.Analog.Basic.Capacitor C(C=0.55e-6) annotation (
          Placement(visible=true, transformation(
            origin={-24,-22},
            extent={{-10,-10},{10,10}},
            rotation=-90)));
      Modelica.Electrical.Analog.Basic.Ground ground annotation (Placement(
            visible=true, transformation(extent={{-74, -64}, {-54, -44}}, rotation=
               0)));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clSw annotation (
          Placement(visible=true, transformation(
            origin={8,-34},
            extent={{-10,-10},{10,10}},
            rotation=-90)));
      Modelica.Blocks.Sources.BooleanStep booleanStep(startTime=0.003,
          startValue=true) annotation (Placement(visible=true, transformation(
              extent={{62, -48}, {42, -28}}, rotation=0)));
      Modelica.Electrical.Analog.Basic.Inductor inductor(L=2.8e-3) annotation (
          Placement(visible=true, transformation(
            origin={-8,32},
            extent={{-10,-10},{10,10}},
            rotation=-90)));
    equation
      connect(Edc.n, C.n) annotation(Line(points = {{-64, -26}, {-64, -36}, {-24, -36}, {-24, -32}}, color = {0, 0, 255}));
      connect(Edc.p, Rbat.p) annotation(Line(points = {{-64, -6}, {-64, 6}}, color = {0, 0, 255}));
      connect(inductor.p, Rbat.n) annotation(Line(points = {{-8, 42}, {-8, 50}, {-64, 50}, {-64, 26}}, color = {0, 0, 255}));
      connect(ground.p, C.n) annotation(Line(points = {{-64, -44}, {-64, -36}, {-24, -36}, {-24, -32}}, color = {0, 0, 255}));
      connect(clSw.p, C.p) annotation(Line(points = {{8, -24}, {8, -2}, {-24, -2}, {-24, -12}}, color = {0, 0, 255}));
      connect(clSw.n, C.n) annotation(Line(points = {{8, -44}, {-24, -44}, {-24, -32}}, color = {0, 0, 255}));
      connect(inductor.n, C.p) annotation(Line(points = {{-8, 22}, {-8, -2}, {-24, -2}, {-24, -12}}, color = {0, 0, 255}));
      connect(clSw.control, booleanStep.y) annotation(Line(points = {{15, -34}, {26, -34}, {26, -38}, {41, -38}}, color = {255, 0, 255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent = {{-80, -60}, {80, 60}}), graphics={Text(origin = {35.35, 1.78}, lineColor = {0, 0, 255}, extent = {{-21.35, 28.22}, {34.65, 8.22}}, textString = "Accensione Motori
    ad Accensione
    Comandata", fontName = "MS Shell Dlg 2", horizontalAlignment = TextAlignment.Left)}),
        Icon(coordinateSystem(extent={{-80,-60},{80,60}}, preserveAspectRatio=
                false)),
        experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>Prima simulazione che mostra il caricamento dell&apos;induttore e la successiva oscillazione R-C.</p>
</html>"),
        __OpenModelica_commandLineOptions="");
    end Cspark0;

    model Cspark1 "Induttore-trasformatore"
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-88,8})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.5) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-88,40})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=2.8e-3,
        M=0.252,
        L2=28.0)
        annotation (Placement(transformation(extent={{-46,44},{-26,64}})));
      Modelica.Electrical.Analog.Basic.Capacitor C(C=0.55e-6) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-48,-4})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-98,-50},{-78,-30}})));
      Modelica.Electrical.Analog.Basic.Resistor plugOff(R=1e7) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={0,52})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{-10,4},{10,24}})));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clSw annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-18,-8})));
      Modelica.Blocks.Sources.BooleanStep booleanStep(startTime=0.003,
          startValue=true)
        annotation (Placement(transformation(extent={{36,-28},{16,-8}})));
    equation
      connect(Edc.n, C.n) annotation (Line(
          points={{-88,-2},{-88,-18},{-48,-18},{-48,-14}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, C.n) annotation (Line(
          points={{-88,-30},{-88,-18},{-48,-18},{-48,-14}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground1.p, plugOff.n) annotation (Line(
          points={{0,24},{0,32},{-1.77636e-015,32},{-1.77636e-015,42}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(clSw.p, C.p) annotation (Line(
          points={{-18,2},{-18,16},{-48,16},{-48,6}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(clSw.n, C.n) annotation (Line(
          points={{-18,-18},{-48,-18},{-48,-14}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-88,18},{-88,30}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p2, plugOff.p) annotation (Line(
          points={{-26,59},{-20,59},{-20,60},{-16,60},{-16,72},{0,72},{0,62},{
              1.77636e-015,62}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.n2, plugOff.n) annotation (Line(
          points={{-26,49},{-24,49},{-24,50},{-14,50},{-14,32},{-1.77636e-015,
              32},{-1.77636e-015,42},{-1.83187e-015,42}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p1, Rbat.n) annotation (Line(
          points={{-46,59},{-68,59},{-68,60},{-88,60},{-88,50}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.n1, C.p) annotation (Line(
          points={{-46,49},{-50,49},{-50,16},{-48,16},{-48,6}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(clSw.control, booleanStep.y) annotation (Line(
          points={{-11,-8},{2,-8},{2,-18},{15,-18}},
          color={255,0,255},
          smooth=Smooth.None));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-60},
                {80,80}}),   graphics={Text(
                  extent={{10,34},{76,28}},
                  lineColor={0,0,255},
              textString="Accensione Motori 
ad Accensione
Comandata")}),
        Icon(coordinateSystem(extent={{-100,-60},{80,80}})),
        experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>Seconda simulazione che mostra l&apos;efficacia del trasformatore per l&apos;elevazione della tensione</p>
</html>"));
    end Cspark1;

    model CsparkOO "Con innesco"
      Real enArc;
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-78,-2})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.5) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-78,30})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=2.8e-3,
        M=0.252,
        L2=28.0)
        annotation (Placement(transformation(extent={{-36,34},{-16,54}})));
      Modelica.Electrical.Analog.Basic.Capacitor C(C=0.55e-6) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-38,-14})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-88,-60},{-68,-40}})));
      Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
            3e-3) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={0,-46})));
      Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
        annotation (Placement(transformation(extent={{18,70},{38,90}})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{-26,10},{-6,30}})));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clSw annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-16,-18})));
      Modelica.Blocks.Math.Abs abs1
        annotation (Placement(transformation(extent={{48,70},{68,90}})));
      Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=15e3)
        annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={100,80})));
      Modelica.Blocks.Logical.Or or1 annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={120,32})));
      Modelica.Blocks.Logical.Pre pre1 annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=-90,
            origin={96,32})));
      Modelica.Blocks.Logical.Pre pre2 "Just to prevent an algebraic loop"
        annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=0,
            origin={98,-12})));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch arcSW(Goff=0.1e-6,
          Ron=30000.0) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={42,44})));
      Modelica.Electrical.Analog.Ideal.IdealDiode diode(Goff=1e-7, Ron=1)
        annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=-90,
            origin={42,16})));
      Modelica.Electrical.Analog.Sources.ConstantVoltage spFem(V=500.0)
        annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={42,-12})));
      Modelica.Electrical.Analog.Basic.Capacitor cOut(C=15e-12) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={24,24})));
    equation
      der(enArc) = arcSW.p.v*arcSW.i;
      connect(Edc.n, C.n) annotation (Line(
          points={{-78,-12},{-78,-28},{-38,-28},{-38,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, C.n) annotation (Line(
          points={{-78,-40},{-78,-28},{-38,-28},{-38,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(clSw.p, C.p) annotation (Line(
          points={{-16,-8},{-16,6},{-38,6},{-38,-4}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(clSw.n, C.n) annotation (Line(
          points={{-16,-28},{-38,-28},{-38,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-78,8},{-78,20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p1, Rbat.n) annotation (Line(
          points={{-36,49},{-58,49},{-58,50},{-78,50},{-78,40}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.n1, C.p) annotation (Line(
          points={{-36,39},{-40,39},{-40,6},{-38,6},{-38,-4}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(abs1.u, vInnerPin.phi) annotation (Line(
          points={{46,80},{39,80}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(vInnerPin.p, transformer.p2) annotation (Line(
          points={{18,80},{4,80},{4,56},{-6,56},{-6,49},{-16,49}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(closeSW.y, clSw.control) annotation (Line(
          points={{8.88178e-016,-35},{8.88178e-016,-18},{-9,-18}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(greaterThreshold.u, abs1.y) annotation (Line(
          points={{88,80},{69,80}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pre1.y, or1.u2) annotation (Line(
          points={{96,43},{96,50},{112,50},{112,44}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(pre1.u, or1.y) annotation (Line(
          points={{96,20},{96,10},{120,10},{120,21}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(pre2.u, or1.y) annotation (Line(
          points={{110,-12},{120,-12},{120,21}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(or1.u1, greaterThreshold.y) annotation (Line(
          points={{120,44},{120,80},{111,80}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(transformer.n2, ground1.p)
        annotation (Line(points={{-16,39},{-16,30}}, color={0,0,255}));
      connect(arcSW.p, transformer.p2) annotation (Line(points={{42,54},{42,54},
              {42,56},{6,56},{-6,56},{-6,49},{-16,49}}, color={0,0,255}));
      connect(arcSW.control, pre2.y) annotation (Line(points={{49,44},{58,44},{
              82,44},{82,-12},{87,-12}}, color={255,0,255}));
      connect(spFem.p, transformer.n2) annotation (Line(points={{42,-22},{10,-22},
              {10,40},{0,40},{0,39},{-16,39}}, color={0,0,255}));
      connect(diode.p, spFem.n)
        annotation (Line(points={{42,6},{42,6},{42,-2}}, color={0,0,255}));
      connect(diode.n, arcSW.n)
        annotation (Line(points={{42,26},{42,30},{42,34}}, color={0,0,255}));
      connect(cOut.p, transformer.p2) annotation (Line(points={{24,34},{24,56},
              {-6,56},{-6,49},{-16,49}}, color={0,0,255}));
      connect(cOut.n, transformer.n2) annotation (Line(points={{24,14},{24,-22},
              {10,-22},{10,40},{-2,40},{-2,39},{-16,39}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-60},
                {140,100}})),
        Icon(coordinateSystem(extent={{-100,-60},{140,100}},
              preserveAspectRatio=false)),
        experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
</html>"));
    end CsparkOO;

    model CsparkOOmdl
      import sparkPkg16;
      Real enArc;
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-76,-6})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.5) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-76,30})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=2.8e-3,
        M=0.252,
        L2=28.0)
        annotation (Placement(transformation(extent={{-40,34},{-20,54}})));
      Modelica.Electrical.Analog.Basic.Capacitor C(C=0.55e-6) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-40,-26})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-98,-62},{-78,-42}})));
      Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
            3e-3) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={20,-30})));
      Modelica.Electrical.Analog.Sensors.PotentialSensor vArc
        annotation (Placement(transformation(extent={{26,40},{44,58}})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{-30,2},{-10,22}})));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch sw annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-10,-30})));
      Modelica.Blocks.Math.Abs abs1
        annotation (Placement(transformation(extent={{60,38},{80,58}})));
      sparkPkg16.ArcActivation arcActiv
        annotation (Placement(transformation(extent={{72,6},{92,26}})));
      sparkPkg16.ArcModel arc(arcRon=30e3) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={40,16})));
      Modelica.Electrical.Analog.Basic.Capacitor cOut(C=15e-12) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={20,16})));
    equation
      der(enArc) = arc.LossPower;
      connect(Edc.n, C.n) annotation (Line(
          points={{-76,-16},{-76,-40},{-40,-40},{-40,-36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, C.n) annotation (Line(
          points={{-88,-42},{-88,-40},{-40,-40},{-40,-36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(sw.p, C.p) annotation (Line(
          points={{-10,-20},{-10,-6},{-40,-6},{-40,-16}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(sw.n, C.n) annotation (Line(
          points={{-10,-40},{-40,-40},{-40,-36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-76,4},{-76,20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p1, Rbat.n) annotation (Line(
          points={{-40,49},{-62,49},{-62,50},{-76,50},{-76,40}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.n1, C.p) annotation (Line(
          points={{-40,39},{-42,39},{-42,-6},{-40,-6},{-40,-16}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(abs1.u, vArc.phi) annotation (Line(
          points={{58,48},{52,48},{52,49},{44.9,49}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(arcActiv.u, abs1.y) annotation (Line(
          points={{82.1,27.3},{82.1,48},{81,48}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(closeSW.y, sw.control) annotation (Line(
          points={{9,-30},{-3,-30}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(ground1.p, transformer.n2)
        annotation (Line(points={{-20,22},{-20,39}}, color={0,0,255}));
      connect(arc.n, ground1.p) annotation (Line(points={{40,6},{40,0},{4,0},{4,
              22},{-20,22}}, color={0,0,255}));
      connect(arcActiv.y, arc.control) annotation (Line(points={{71,16},{71,16},
              {47.4,16}}, color={255,0,255}));
      connect(cOut.n, ground1.p) annotation (Line(points={{20,6},{20,0},{4,0},{
              4,22},{-20,22}}, color={0,0,255}));
      connect(arc.p, cOut.p) annotation (Line(points={{40,26},{40,36},{20,36},{
              20,26}}, color={0,0,255}));
      connect(cOut.p, vArc.p) annotation (Line(points={{20,26},{20,26},{20,49},
              {26,49}}, color={0,0,255}));
      connect(vArc.p, transformer.p2)
        annotation (Line(points={{26,49},{-20,49}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-60},
                {100,80}})),
        Icon(coordinateSystem(extent={{-100,-60},{100,80}})),
        experiment(
          StopTime=0.008,
          StartTime=0,
          Tolerance=0.0001,
          Interval=4e-06),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
</html>"));
    end CsparkOOmdl;

    model sparkOOResis "OO with added resistance"
      import sparkPkg16;
      Real enArc;
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-76,-6})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.5) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-76,30})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=2.8e-3,
        M=0.252,
        L2=28.0)
        annotation (Placement(transformation(extent={{-40,34},{-20,54}})));
      Modelica.Electrical.Analog.Basic.Capacitor C(C=0.55e-6) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-40,-26})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-98,-62},{-78,-42}})));
      Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
            3e-3) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=180,
            origin={20,-30})));
      Modelica.Electrical.Analog.Sensors.PotentialSensor vArc
        annotation (Placement(transformation(extent={{28,40},{46,58}})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{-30,2},{-10,22}})));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch sw annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-10,-30})));
      Modelica.Blocks.Math.Abs abs1 annotation (Placement(visible=true,
            transformation(extent={{60,36},{80,56}}, rotation=0)));
      sparkPkg16.ArcActivation arcActiv
        annotation (Placement(transformation(extent={{72,6},{92,26}})));
      sparkPkg16.ArcModel arc(arcRon=15e3) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={46,16})));
      Modelica.Electrical.Analog.Basic.Capacitor cOut(C=15e-12) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={26,16})));
      Modelica.Electrical.Analog.Basic.Resistor r1(R=15e3)
        "Wire and secondary resistance"
        annotation (Placement(transformation(extent={{-10,44},{0,54}})));
    equation
      connect(arcActiv.u, abs1.y) annotation (Line(points={{82.1,27.3},{82.1,46},
              {81,46}}, color={0,0,127}));
      connect(abs1.u, vArc.phi) annotation (Line(points={{58,46},{52,46},{52,49},
              {46.9,49}}, color={0,0,127}));
      der(enArc) = arc.LossPower;
      connect(Edc.n, C.n) annotation (Line(
          points={{-76,-16},{-76,-40},{-40,-40},{-40,-36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, C.n) annotation (Line(
          points={{-88,-42},{-88,-40},{-40,-40},{-40,-36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(sw.p, C.p) annotation (Line(
          points={{-10,-20},{-10,-6},{-40,-6},{-40,-16}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(sw.n, C.n) annotation (Line(
          points={{-10,-40},{-40,-40},{-40,-36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-76,4},{-76,20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p1, Rbat.n) annotation (Line(
          points={{-40,49},{-62,49},{-62,50},{-76,50},{-76,40}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.n1, C.p) annotation (Line(
          points={{-40,39},{-42,39},{-42,-6},{-40,-6},{-40,-16}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(closeSW.y, sw.control) annotation (Line(
          points={{9,-30},{-3,-30}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(ground1.p, transformer.n2)
        annotation (Line(points={{-20,22},{-20,39}}, color={0,0,255}));
      connect(arc.n, ground1.p) annotation (Line(points={{46,6},{46,0},{10,0},{
              10,22},{-20,22}}, color={0,0,255}));
      connect(arcActiv.y, arc.control) annotation (Line(points={{71,16},{71,16},
              {53.4,16}}, color={255,0,255}));
      connect(cOut.n, ground1.p) annotation (Line(points={{26,6},{26,0},{10,0},
              {10,22},{-20,22}}, color={0,0,255}));
      connect(transformer.p2, r1.p)
        annotation (Line(points={{-20,49},{-10,49}}, color={0,0,255}));
      connect(arc.p, cOut.p) annotation (Line(points={{46,26},{46,34},{26,34},{
              26,26}}, color={0,0,255}));
      connect(r1.n, vArc.p) annotation (Line(points={{0,49},{14,49},{14,49},{28,
              49}}, color={0,0,255}));
      connect(vArc.p, cOut.p) annotation (Line(points={{28,49},{14,49},{14,34},
              {14,34},{26,34},{26,26}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-60},
                {100,60}})),
        Icon(coordinateSystem(extent={{-100,-60},{100,80}})),
        experiment(
          StopTime=0.008,
          StartTime=0,
          Tolerance=0.0001,
          Interval=4e-06),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>Aggiunta resistenza delle connessione, come da riferimenti bibliografici.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
</html>"),
        __OpenModelica_commandLineOptions="");
    end sparkOOResis;

    package OLD
      model sparkGen
        Real enArc;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-78,-2})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-78,30})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-36,34},{-16,54}})));
        Modelica.Electrical.Analog.Basic.Capacitor C(C=0.5e-6) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-38,-14})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-88,-60},{-68,-40}})));
        Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
              3e-3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={16,-36})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{18,70},{38,90}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-8,6},{12,26}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clSw annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-8,-18})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{48,70},{68,90}})));
        Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=
              15e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={100,80})));
        Modelica.Blocks.Logical.Or or1 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={120,32})));
        Modelica.Blocks.Logical.Pre pre1 annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=-90,
              origin={96,32})));
        Modelica.Blocks.Logical.Pre pre2 "Just to prevent an algebraic loop"
          annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=0,
              origin={76,0})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clArc(Goff=0.1e-6,
            Ron=40e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={28,44})));
      equation
        der(enArc) = clArc.v*clArc.i;
        connect(Edc.n, C.n) annotation (Line(
            points={{-78,-12},{-78,-28},{-38,-28},{-38,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, C.n) annotation (Line(
            points={{-78,-40},{-78,-28},{-38,-28},{-38,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(clSw.p, C.p) annotation (Line(
            points={{-8,-8},{-8,6},{-38,6},{-38,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(clSw.n, C.n) annotation (Line(
            points={{-8,-28},{-38,-28},{-38,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-78,8},{-78,20}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-36,49},{-58,49},{-58,50},{-78,50},{-78,40}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.n1, C.p) annotation (Line(
            points={{-36,39},{-40,39},{-40,6},{-38,6},{-38,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{46,80},{39,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{18,80},{4,80},{4,56},{-6,56},{-6,49},{-16,49}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(closeSW.y, clSw.control) annotation (Line(
            points={{16,-25},{16,-18},{-1,-18}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(greaterThreshold.u, abs1.y) annotation (Line(
            points={{88,80},{69,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(pre1.y, or1.u2) annotation (Line(
            points={{96,43},{96,50},{112,50},{112,44}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(pre1.u, or1.y) annotation (Line(
            points={{96,20},{96,10},{120,10},{120,21}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(pre2.u, or1.y) annotation (Line(
            points={{88,0},{120,0},{120,21}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(or1.u1, greaterThreshold.y) annotation (Line(
            points={{120,44},{120,80},{111,80}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(transformer.n2, ground1.p) annotation (Line(points={{-16,39},{-16,
                26},{2,26}}, color={0,0,255}));
        connect(ground1.p, clArc.n) annotation (Line(points={{2,26},{16,26},{28,
                26},{28,34}}, color={0,0,255}));
        connect(clArc.p, transformer.p2) annotation (Line(points={{28,54},{28,
                54},{28,56},{4,56},{-6,56},{-6,49},{-16,49}}, color={0,0,255}));
        connect(clArc.control, pre2.y) annotation (Line(points={{35,44},{44,44},
                {48,44},{48,0},{65,0}}, color={255,0,255}));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-80},
                  {140,100}}), graphics={Text(
                      extent={{-36,-60},{80,-70}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Manca lo strappo della corrente a fine arco")}),
          Icon(coordinateSystem(extent={{-100,-80},{140,100}},
                preserveAspectRatio=false)),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
</html>"));
      end sparkGen;

      model sparkGen1
        import sparkPkg16;
        Real enArc;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-88,-2})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-88,30})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-46,34},{-26,54}})));
        Modelica.Electrical.Analog.Basic.Capacitor C(C=0.5e-6) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-48,-14})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-98,-60},{-78,-40}})));
        Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
              3e-3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={6,-36})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{8,70},{28,90}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-18,6},{2,26}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch sw annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-18,-18})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{38,70},{58,90}})));
        sparkPkg16.ArcActivation arcActiv
          annotation (Placement(transformation(extent={{70,32},{90,52}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clArc(Goff=0.1e-6,
            Ron=40e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={14,42})));
      equation
        der(enArc) = clArc.LossPower;
        connect(Edc.n, C.n) annotation (Line(
            points={{-88,-12},{-88,-28},{-48,-28},{-48,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, C.n) annotation (Line(
            points={{-88,-40},{-88,-28},{-48,-28},{-48,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.p, C.p) annotation (Line(
            points={{-18,-8},{-18,6},{-48,6},{-48,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.n, C.n) annotation (Line(
            points={{-18,-28},{-48,-28},{-48,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-88,8},{-88,20}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-46,49},{-68,49},{-68,50},{-88,50},{-88,40}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.n1, C.p) annotation (Line(
            points={{-46,39},{-50,39},{-50,6},{-48,6},{-48,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{36,80},{29,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{8,80},{-6,80},{-6,56},{-16,56},{-16,49},{-26,49}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(arcActiv.u, abs1.y) annotation (Line(
            points={{80.1,53.3},{80.1,80},{59,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(closeSW.y, sw.control) annotation (Line(
            points={{6,-25},{6,-18},{-11,-18}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(clArc.n, ground1.p)
          annotation (Line(points={{14,32},{14,26},{-8,26}}, color={0,0,255}));
        connect(ground1.p, transformer.n2) annotation (Line(points={{-8,26},{-26,
                26},{-26,39}}, color={0,0,255}));
        connect(clArc.p, transformer.p2) annotation (Line(points={{14,52},{14,
                56},{-16,56},{-16,49},{-26,49}}, color={0,0,255}));
        connect(clArc.control, arcActiv.y) annotation (Line(points={{21,42},{
                45.5,42},{69,42}}, color={255,0,255}));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-80},
                  {140,100}}), graphics={Text(
                      extent={{4,-62},{120,-72}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Manca lo strappo della corrente a fine arco")}),
          Icon(coordinateSystem(extent={{-100,-80},{140,100}})),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
</html>"));
      end sparkGen1;

      model ArcLogic
        Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=
              15e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={70,54})));
        Modelica.Blocks.Logical.Or or1 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={86,16})));
        Modelica.Blocks.Logical.Pre pre1 annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=-90,
              origin={62,16})));
        Modelica.Blocks.Logical.Pre pre2 "Just to prevent an algebraic loop"
          annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=0,
              origin={76,-40})));
        Modelica.Blocks.Interfaces.BooleanOutput close annotation (Placement(
              transformation(rotation=0, extent={{-100,50},{-120,70}})));
        Modelica.Blocks.Interfaces.RealInput u annotation (Placement(
              transformation(
              rotation=-90,
              extent={{-13,-13},{13,13}},
              origin={61,113})));
        Modelica.Blocks.Interfaces.BooleanOutput open annotation (Placement(
              transformation(rotation=0, extent={{-100,-70},{-120,-50}})));
        Modelica.Blocks.Math.Mean mean(f=filtFreq) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-20,28})));
        Modelica.Blocks.Logical.LessThreshold lessThreshold(threshold=currThres)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-20,-2})));
        Modelica.Blocks.Logical.And and1 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-12,-40})));
        Modelica.Blocks.Math.Abs abs2 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-20,78})));
        Modelica.Blocks.MathBoolean.OnDelay onDelay(delayTime=2/filtFreq)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={20,-2})));
        Modelica.Blocks.Interfaces.RealInput i annotation (Placement(
              transformation(
              rotation=-90,
              extent={{-13,-13},{13,13}},
              origin={-59,113})));
        parameter Modelica.SIunits.Frequency filtFreq=10e3;
        parameter Modelica.SIunits.Current currThres=10e-3;
      equation
        connect(or1.u1, greaterThreshold.y) annotation (Line(
            points={{86,28},{86,54},{81,54}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(pre1.y, or1.u2) annotation (Line(
            points={{62,27},{62,34},{78,34},{78,28}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(pre1.u, or1.y) annotation (Line(
            points={{62,4},{62,-6},{86,-6},{86,5}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(pre2.u, or1.y) annotation (Line(
            points={{88,-40},{98,-40},{98,-6},{86,-6},{86,5}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(u, greaterThreshold.u) annotation (Line(points={{61,113},{61,94},
                {74,94},{74,74},{48,74},{48,54},{58,54}}, color={0,0,127}));
        connect(pre2.y, close) annotation (Line(
            points={{65,-40},{46,-40},{46,60},{-110,60}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(mean.y, lessThreshold.u) annotation (Line(
            points={{-20,17},{-20,10}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(lessThreshold.y, and1.u2) annotation (Line(
            points={{-20,-13},{-20,-28}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(mean.u, abs2.y) annotation (Line(
            points={{-20,40},{-20,67}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(onDelay.y, and1.u1) annotation (Line(
            points={{20,-14},{20,-20},{-12,-20},{-12,-28}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(abs2.u, i) annotation (Line(
            points={{-20,90},{-20,92},{-59,92},{-59,113}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(onDelay.u, close) annotation (Line(
            points={{20,12},{20,60},{-110,60}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(and1.y, open) annotation (Line(
            points={{-12,-51},{-12,-60},{-110,-60}},
            color={255,0,255},
            smooth=Smooth.None));
        annotation (Diagram(coordinateSystem(extent={{-100,-100},{100,100}},
                preserveAspectRatio=false), graphics={Text(
                      extent={{-260,-94},{-144,-104}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Aggiunto primo modello per lo strappo della corrente a fine arco 
con attivazione a soglia di corrente")}), Icon(coordinateSystem(extent={{-100,-100},
                  {100,100}}, preserveAspectRatio=false)));
      end ArcLogic;

      model sparkExtinctTimed
        import sparkPkg16;
        Real enArc;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-88,-2})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-88,30})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-46,34},{-26,54}})));
        Modelica.Electrical.Analog.Basic.Capacitor C(C=0.5e-6) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-50,-14})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-98,-60},{-78,-40}})));
        Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
              3e-3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={0,-42})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{8,70},{28,90}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-36,8},{-16,28}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch sw annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-18,-18})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{38,70},{58,90}})));
        Modelica.Electrical.Analog.Sensors.CurrentSensor iArc
          annotation (Placement(transformation(extent={{70,16},{50,36}})));
        Modelica.Blocks.Logical.Timer timer annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={88,-38})));
        Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=
              0.001) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={54,-38})));
        sparkPkg16.ArcActivation arcActivate
          annotation (Placement(transformation(extent={{112,36},{132,56}})));
        Modelica.Electrical.Analog.Ideal.OpenerWithArc openArc(
          Goff=0.1e-6,
          Vmax=1e4,
          V0=0,
          dVdt=10e6)
          annotation (Placement(transformation(extent={{16,36},{36,16}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clArc(Goff=0.1e-6,
            Ron=40e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={82,36})));
      equation
        der(enArc) = clArc.v*clArc.i;
        connect(Edc.n, C.n) annotation (Line(
            points={{-88,-12},{-88,-28},{-50,-28},{-50,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, C.n) annotation (Line(
            points={{-88,-40},{-88,-28},{-50,-28},{-50,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.p, C.p) annotation (Line(
            points={{-18,-8},{-18,6},{-50,6},{-50,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.n, C.n) annotation (Line(
            points={{-18,-28},{-50,-28},{-50,-24}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-88,8},{-88,20}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-46,49},{-68,49},{-68,50},{-88,50},{-88,40}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.n1, C.p) annotation (Line(
            points={{-46,39},{-50,39},{-50,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{36,80},{29,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(timer.y, greaterThreshold.u) annotation (Line(
            points={{77,-38},{66,-38}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(sw.control, closeSW.y) annotation (Line(
            points={{-11,-18},{6.66134e-016,-18},{6.66134e-016,-31}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(abs1.y, arcActivate.u) annotation (Line(
            points={{59,80},{122,80},{122,57.3},{122.1,57.3}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(ground1.p, transformer.n2) annotation (Line(
            points={{-26,28},{-26,39}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{8,80},{-6,80},{-6,56},{-26,56},{-26,49}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(iArc.n, openArc.n) annotation (Line(
            points={{50,26},{36,26}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(openArc.p, transformer.n2) annotation (Line(
            points={{16,26},{-14,26},{-14,34},{-26,34},{-26,39}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(greaterThreshold.y, openArc.control) annotation (Line(
            points={{43,-38},{26,-38},{26,16}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(iArc.p, clArc.n)
          annotation (Line(points={{70,26},{82,26}}, color={0,0,255}));
        connect(clArc.p, transformer.p2) annotation (Line(points={{82,46},{82,
                56},{-26,56},{-26,49}}, color={0,0,255}));
        connect(clArc.control, arcActivate.y) annotation (Line(points={{89,36},
                {106,36},{106,46},{111,46}}, color={255,0,255}));
        connect(timer.u, arcActivate.y) annotation (Line(points={{100,-38},{100,
                -38},{106,-38},{106,46},{111,46}}, color={255,0,255}));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                  {140,100}}), graphics={Text(
                      extent={{-40,-70},{76,-80}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Aggiunto primo modello per lo strappo della corrente a fine arco con attivazione a tempo
"),Text(              extent={{-62,-82},{120,-98}},
                      lineColor={255,0,0},
                      textString=
                  "Tema proposto: openArc come impedenza crescente"),Text(
                      extent={{-94,96},{-36,80}},
                      lineColor={28,108,200},
                      textString="Non fatto 27-11-05")}),
          Icon(coordinateSystem(extent={{-100,-100},{140,100}})),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene mediante un secondo interruttore, pilotato per il momento sulla base del tempo. Un modello pi&ugrave; realistico &egrave; invece corrispondente al raggiungimento di una soglia minima di corrente iHold (modelli successivi)</p>
</html>"));
      end sparkExtinctTimed;

      model sparkExtinctCurrent
        import sparkPkg16;
        Real enArc, enSw;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-88,6})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-88,38})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-46,42},{-26,62}})));
        Modelica.Electrical.Analog.Basic.Capacitor C(C=0.5e-6) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-50,-6})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-98,-46},{-78,-26}})));
        Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
              3e-3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-6,-34})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{10,76},{30,96}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-36,16},{-16,36}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch sw(Ron=1e-3, Goff=
              1e-3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-18,-10})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{40,76},{60,96}})));
        Modelica.Electrical.Analog.Sensors.CurrentSensor iArc
          annotation (Placement(transformation(extent={{74,24},{54,44}})));
        sparkPkg16.ArcActivation arcActivation
          annotation (Placement(transformation(extent={{112,44},{132,64}})));
        Modelica.Electrical.Analog.Ideal.OpenerWithArc openArc(
          Goff=0.1e-6,
          dVdt=1e7,
          Vmax=1e4,
          V0=0) annotation (Placement(transformation(extent={{16,44},{36,24}})));
        Modelica.Blocks.Math.Mean mean(f=10e3) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={64,-16})));
        Modelica.Blocks.Logical.LessThreshold lessThreshold(threshold=0.01)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={64,-46})));
        Modelica.Blocks.Logical.And and1 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={44,-74})));
        Modelica.Blocks.Math.Abs abs2 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={64,8})));
        Modelica.Blocks.MathBoolean.OnDelay onDelay(delayTime=0.2e-3)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={104,-22})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clArc(Goff=0.1e-6,
            Ron=40e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={82,54})));
      equation
        der(enSw) = sw.LossPower;
        der(enArc) = clArc.v*clArc.i;
        connect(Edc.n, C.n) annotation (Line(
            points={{-88,-4},{-88,-20},{-50,-20},{-50,-16}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, C.n) annotation (Line(
            points={{-88,-26},{-88,-20},{-50,-20},{-50,-16}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.p, C.p) annotation (Line(
            points={{-18,0},{-18,14},{-50,14},{-50,4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.n, C.n) annotation (Line(
            points={{-18,-20},{-50,-20},{-50,-16}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-88,16},{-88,28}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-46,57},{-68,57},{-68,58},{-88,58},{-88,48}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.n1, C.p) annotation (Line(
            points={{-46,47},{-50,47},{-50,4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{38,86},{34,86},{34,84},{32,84},{32,86},{31,86}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(sw.control, closeSW.y) annotation (Line(
            points={{-11,-10},{-6,-10},{-6,-23}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(abs1.y, arcActivation.u) annotation (Line(
            points={{61,86},{122,86},{122,65.3},{122.1,65.3}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(ground1.p, transformer.n2) annotation (Line(
            points={{-26,36},{-26,47}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{10,86},{-6,86},{-6,64},{-26,64},{-26,57}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(iArc.n, openArc.n) annotation (Line(
            points={{54,34},{36,34}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(openArc.p, transformer.n2) annotation (Line(
            points={{16,34},{-14,34},{-14,42},{-26,42},{-26,47}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(mean.y, lessThreshold.u) annotation (Line(
            points={{64,-27},{64,-34}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(lessThreshold.y, and1.u2) annotation (Line(
            points={{64,-57},{64,-66},{56,-66}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(mean.u, abs2.y) annotation (Line(
            points={{64,-4},{64,-3}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(abs2.u, iArc.i) annotation (Line(
            points={{64,20},{64,24}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(onDelay.y, and1.u1) annotation (Line(
            points={{104,-34},{104,-74},{56,-74}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(and1.y, openArc.control) annotation (Line(
            points={{33,-74},{26,-74},{26,24}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(clArc.p, transformer.p2) annotation (Line(points={{82,64},{-26,
                64},{-26,57}}, color={0,0,255}));
        connect(clArc.control, arcActivation.y)
          annotation (Line(points={{89,54},{111,54}}, color={255,0,255}));
        connect(onDelay.u, arcActivation.y) annotation (Line(points={{104,-8},{
                104,-8},{104,38},{104,54},{111,54}}, color={255,0,255}));
        connect(clArc.n, iArc.p)
          annotation (Line(points={{82,44},{82,34},{74,34}}, color={0,0,255}));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-100},
                  {140,100}}), graphics={Text(
                      extent={{-94,-64},{22,-74}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Con modello per lo strappo della corrente a fine arco 
con attivazione a soglia di corrente"),Text(
                      extent={{-84,-98},{98,-114}},
                      lineColor={255,0,0},
                      textString=
                  "Tema proposto: openArc.quenched fra gli output")}),
          Icon(coordinateSystem(extent={{-100,-100},{140,100}})),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello si sono integrati due modelli di arco: l&apos;innesco dell&apos;arco con tensione costante ins ereie con resistenza, con closeArc; l&apos;estinzione dell&apos;arco con tensione crescente che forza la corrente a 0 con openArc.</p>
<p>La logica di interruzione dell&apos;arco &egrave; realizzata sulla base del raggiungimento di una soglia minima di corrente.</p>
<p>Questa logica &egrave; per&ograve; incapsulata solo parzialmente: manca di mettere in un unico controllore anche la lgcica di openArc.</p>
</html>"));
      end sparkExtinctCurrent;

      model sparkExtCurrLogic
        Real enArc;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-96,-20})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-96,12})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-54,16},{-34,36}})));
        Modelica.Electrical.Analog.Basic.Capacitor C(C=0.5e-6) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-58,-32})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-106,-78},{-86,-58}})));
        Modelica.Blocks.Sources.BooleanStep closeSW(startValue=true, startTime=
              3e-3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-22,-60})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{0,52},{20,72}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-44,-10},{-24,10}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch sw annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-34,-36})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{30,52},{50,72}})));
        Modelica.Electrical.Analog.Sensors.CurrentSensor iArc annotation (
            Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=180,
              origin={28,28})));
        Modelica.Electrical.Analog.Ideal.OpenerWithArc openArc(
          Goff=0.1e-6,
          dVdt=1e7,
          Vmax=1e4,
          V0=0) annotation (Placement(transformation(extent={{8,18},{28,-2}})));
        OLD.ArcLogic arcLogic
          annotation (Placement(transformation(extent={{84,-36},{104,-16}})));
        Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clArc(Goff=0.1e-6,
            Ron=40e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={50,8})));
      equation
        der(enArc) = clArc.v*clArc.i;
        connect(Edc.n, C.n) annotation (Line(
            points={{-96,-30},{-96,-46},{-58,-46},{-58,-42}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, C.n) annotation (Line(
            points={{-96,-58},{-96,-46},{-58,-46},{-58,-42}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.p, C.p) annotation (Line(
            points={{-34,-26},{-34,-12},{-58,-12},{-58,-22}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(sw.n, C.n) annotation (Line(
            points={{-34,-46},{-58,-46},{-58,-42}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-96,-10},{-96,2}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-54,31},{-76,31},{-76,32},{-96,32},{-96,22}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.n1, C.p) annotation (Line(
            points={{-54,21},{-58,21},{-58,-22}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{28,62},{21,62}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(sw.control, closeSW.y) annotation (Line(
            points={{-27,-36},{-22,-36},{-22,-49}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(ground1.p, transformer.n2) annotation (Line(
            points={{-34,10},{-34,21}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{0,62},{-14,62},{-14,38},{-34,38},{-34,31}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(openArc.p, transformer.n2) annotation (Line(
            points={{8,8},{-22,8},{-22,16},{-34,16},{-34,21}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(arcLogic.u, abs1.y) annotation (Line(
            points={{100.1,-14.7},{100.1,62},{51,62}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(iArc.i, arcLogic.i) annotation (Line(
            points={{28,38},{88,38},{88,12},{88.1,12},{88.1,-14.7}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(arcLogic.open, openArc.control) annotation (Line(
            points={{83,-32},{18,-32},{18,-2}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(iArc.p, transformer.p2) annotation (Line(
            points={{18,28},{-14,28},{-14,38},{-34,38},{-34,31}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(openArc.n, clArc.n)
          annotation (Line(points={{28,8},{34,8},{40,8}}, color={0,0,255}));
        connect(clArc.p, iArc.n) annotation (Line(points={{60,8},{70,8},{70,28},
                {38,28}}, color={0,0,255}));
        connect(arcLogic.close, clArc.control) annotation (Line(points={{83,-20},
                {66,-20},{50,-20},{50,1}}, color={255,0,255}));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-120,-80},
                  {120,80}}), graphics={Text(
                      extent={{14,-56},{118,-68}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Aggiunto primo modello per lo strappo della corrente 
a fine arco con attivazione a soglia di corrente")}),
          Icon(coordinateSystem(extent={{-120,-80},{120,80}},
                preserveAspectRatio=false)),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello si sono integrati due modelli di arco: l&apos;innesco dell&apos;arco con tensione costante ins ereie con resistenza, con closeArc; l&apos;estinzione dell&apos;arco con tensione crescente che forza la corrente a 0 con openArc.</p>
<p>La logica &egrave; incapsulata in un unico controllore.</p>
<p> OM r23607 OK </p>
</html>"));
      end sparkExtCurrLogic;
    end OLD;
  end C_based;

  package VarResBreakerBased
    model Ispark0
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-38,-2})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-38,30})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-48,-60},{-28,-40}})));
      Modelica.Electrical.Analog.Basic.Inductor inductor(L=2.8e-3, i(start=
              4.6667)) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={18,46})));
      Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={18,0})));
      Modelica.Blocks.Sources.Ramp ramp(
        height=1000.0,
        offset=1,
        duration=100e-6,
        startTime=0.0001)
        annotation (Placement(transformation(extent={{64,-10},{44,10}})));
    equation
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-38,8},{-38,20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(inductor.p, Rbat.n) annotation (Line(
          points={{18,56},{18,66},{-38,66},{-38,40}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(inductor.n, igbt.p) annotation (Line(
          points={{18,36},{18,10}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.n, Edc.n) annotation (Line(
          points={{18,-10},{18,-28},{-38,-28},{-38,-12}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, Edc.n) annotation (Line(
          points={{-38,-40},{-38,-12}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.R, ramp.y) annotation (Line(
          points={{29,0},{43,0}},
          color={0,0,127},
          smooth=Smooth.None));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-60},
                {140,80}}), graphics),
        Icon(coordinateSystem(extent={{-100,-60},{140,80}})),
        experiment(StopTime=0.0003, __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a 15 kV.</p>
<p>Per semplicit&agrave; si fa estinguere in contemporanea con la richiusura dl ruttore mentre nella realt&agrave; l&apos;estinzione avviene prima</p>
</html>"));
    end Ispark0;

    model Ispark1
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-54,14})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.0) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-54,46})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-64,-44},{-44,-24}})));
      Modelica.Electrical.Analog.Basic.Resistor plugOff(R=1e7) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={34,62})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{56,24},{76,44}})));
      Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-12,6})));
      Modelica.Blocks.Sources.Ramp ramp(
        height=1000.0,
        offset=1,
        duration=100e-6,
        startTime=0.0001)
        annotation (Placement(transformation(extent={{34,-4},{14,16}})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=6e-3,
        L2=55.0,
        M=0.99*sqrt(6e-3*55),
        i1(fixed=true, start=6))
        annotation (Placement(transformation(extent={{-8,54},{12,74}})));
    equation
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-54,24},{-54,36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.n, ground.p) annotation (Line(
          points={{-12,-4},{-12,-16},{-54,-16},{-54,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(Edc.n, ground.p) annotation (Line(
          points={{-54,4},{-54,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground1.p, plugOff.n) annotation (Line(
          points={{66,44},{34,44},{34,52}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.R, ramp.y)
        annotation (Line(points={{-1,6},{6,6},{13,6}}, color={0,0,127}));
      connect(transformer.p2, plugOff.p) annotation (Line(points={{12,69},{24,
              69},{24,72},{34,72}}, color={0,0,255}));
      connect(transformer.n2, plugOff.n) annotation (Line(points={{12,59},{24,
              59},{24,44},{34,44},{34,52}}, color={0,0,255}));
      connect(transformer.p1, Rbat.n)
        annotation (Line(points={{-8,69},{-54,69},{-54,56}}, color={0,0,255}));
      connect(transformer.n1, igbt.p)
        annotation (Line(points={{-8,59},{-12,59},{-12,16}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-40},
                {100,100}})),
        Icon(coordinateSystem(extent={{-100,-40},{100,100}})),
        experiment(StopTime=0.0003, __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a 15 kV.</p>
<p>Per semplicit&agrave; si fa estinguere in contemporanea con la richiusura dl ruttore mentre nella realt&agrave; l&apos;estinzione avviene prima</p>
</html>"));
    end Ispark1;

    model iFig4 "Figure 4 of paper"
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-54,14})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2.0) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-54,46})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-64,-44},{-44,-24}})));
      Modelica.Electrical.Analog.Basic.Resistor plugOff(R=1e7) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={40,62})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{62,24},{82,44}})));
      Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-8,22})));
      Modelica.Blocks.Sources.Ramp ramp(
        height=1000.0,
        duration=0.00005,
        startTime=0.005)
        annotation (Placement(transformation(extent={{40,-4},{20,16}})));
      Modelica.Electrical.Analog.Semiconductors.ZDiode zDiode(Maxexp=10, Bv=400)
        annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-24,22})));
      Modelica.Electrical.Analog.Basic.Transformer transformer1(
        L1=6e-3,
        L2=55.0,
        M=0.99*sqrt(6e-3*55),
        i1(fixed=true, start=6))
        annotation (Placement(transformation(extent={{-8,50},{12,70}})));
      Real zenerEnergy;
    equation
      der(zenerEnergy) = zDiode.LossPower;
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-54,24},{-54,36}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.n, ground.p) annotation (Line(
          points={{-8,12},{-8,-16},{-54,-16},{-54,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(Edc.n, ground.p) annotation (Line(
          points={{-54,4},{-54,-24}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ramp.y, igbt.R) annotation (Line(
          points={{19,6},{12,6},{12,22},{3,22}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(ground1.p, plugOff.n) annotation (Line(
          points={{72,44},{40,44},{40,52}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(zDiode.p, ground.p) annotation (Line(points={{-24,12},{-24,12},{-24,
              0},{-8,0},{-8,-16},{-54,-16},{-54,-24}}, color={0,0,255}));
      connect(plugOff.p, transformer1.p2) annotation (Line(points={{40,72},{26,
              72},{26,65},{12,65}}, color={0,0,255}));
      connect(plugOff.n, transformer1.n2) annotation (Line(points={{40,52},{26,
              52},{26,55},{12,55}}, color={0,0,255}));
      connect(transformer1.p1, Rbat.n) annotation (Line(points={{-8,65},{-32,65},
              {-32,66},{-54,66},{-54,56}}, color={0,0,255}));
      connect(transformer1.n1, zDiode.n) annotation (Line(points={{-8,55},{-8,
              55},{-8,40},{-24,40},{-24,32}}, color={0,0,255}));
      connect(transformer1.n1, igbt.p)
        annotation (Line(points={{-8,55},{-8,55},{-8,32}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-40},
                {100,100}}), graphics={Text(
                  extent={{-8,86},{16,84}},
                  lineColor={0,0,0},
                  textString="transf param 
come Table1 
del riferim.")}),
        Icon(coordinateSystem(extent={{-100,-40},{100,100}})),
        experiment(
          StartTime=0.004,
          StopTime=0.0055,
          __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a 15 kV.</p>
<p>Per semplicit&agrave; si fa estinguere in contemporanea con la richiusura dl ruttore mentre nella realt&agrave; l&apos;estinzione avviene prima</p>
</html>"));
    end iFig4;

    model IsparkOO
      Real enArc;
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-100,-2})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-100,30})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=2.8e-3,
        M=0.252,
        L2=28.0)
        annotation (Placement(transformation(extent={{-60,38},{-40,58}})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-110,-60},{-90,-40}})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{-50,6},{-30,26}})));
      Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-60,-22})));
      Modelica.Blocks.Sources.Ramp ramp(
        startTime=0.003,
        height=1000.0,
        duration=0.0002)
        annotation (Placement(transformation(extent={{-14,-32},{-34,-12}})));
      Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
        annotation (Placement(transformation(extent={{4,68},{24,88}})));
      Modelica.Blocks.Math.Abs abs1
        annotation (Placement(transformation(extent={{34,68},{54,88}})));
      Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=15e3)
        annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=0,
            origin={84,78})));
      Modelica.Blocks.Logical.Or or1 annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={104,30})));
      Modelica.Blocks.Logical.Pre pre1 annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=-90,
            origin={80,30})));
      Modelica.Blocks.Logical.Pre pre2 "Just to prevent an algebraic loop"
        annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=0,
            origin={82,-14})));
      Modelica.Electrical.Analog.Ideal.IdealClosingSwitch arcSW(Goff=0.1e-6,
          Ron=30000) annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={26,42})));
      Modelica.Electrical.Analog.Ideal.IdealDiode diode(Ron=1e-7, Goff=1e-7)
        annotation (Placement(transformation(
            extent={{10,-10},{-10,10}},
            rotation=-90,
            origin={26,14})));
      Modelica.Electrical.Analog.Sources.ConstantVoltage spFem(V=400.0)
        annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={26,-14})));
      Modelica.Electrical.Analog.Basic.Capacitor cOut(C=15e-12) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={8,16})));
    equation
      der(enArc) =arcSW.p.v*arcSW.i;
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-100,8},{-100,20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p1, Rbat.n) annotation (Line(
          points={{-60,53},{-100,53},{-100,50},{-100,50},{-100,40}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.p, transformer.n1) annotation (Line(
          points={{-60,-12},{-60,43}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.n, Edc.n) annotation (Line(
          points={{-60,-32},{-100,-32},{-100,-12}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, Edc.n) annotation (Line(
          points={{-100,-40},{-100,-12}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ramp.y, igbt.R) annotation (Line(
          points={{-35,-22},{-49,-22}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(abs1.u, vInnerPin.phi) annotation (Line(
          points={{32,78},{30,78},{30,78},{26,78},{26,78},{25,78}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(vInnerPin.p, transformer.p2) annotation (Line(
          points={{4,78},{-10,78},{-10,54},{-20,54},{-20,53},{-40,53}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(greaterThreshold.u, abs1.y) annotation (Line(
          points={{72,78},{55,78}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(pre1.y, or1.u2) annotation (Line(
          points={{80,41},{80,48},{96,48},{96,42}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(pre1.u, or1.y) annotation (Line(
          points={{80,18},{80,8},{104,8},{104,19}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(pre2.u, or1.y) annotation (Line(
          points={{94,-14},{104,-14},{104,19}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(or1.u1, greaterThreshold.y) annotation (Line(
          points={{104,42},{104,78},{95,78}},
          color={255,0,255},
          smooth=Smooth.None));
      connect(arcSW.p, transformer.p2) annotation (Line(points={{26,52},{26,52},
              {26,54},{-10,54},{-20,54},{-20,53},{-40,53}}, color={0,0,255}));
      connect(arcSW.control, pre2.y) annotation (Line(points={{33,42},{33,42},{
              64,42},{64,-14},{71,-14}}, color={255,0,255}));
      connect(spFem.p, transformer.n2) annotation (Line(points={{26,-24},{-4,-24},
              {-4,-22},{-4,44},{-22,44},{-22,43},{-40,43}}, color={0,0,255}));
      connect(diode.p, spFem.n)
        annotation (Line(points={{26,4},{26,4},{26,-4}}, color={0,0,255}));
      connect(diode.n, arcSW.n)
        annotation (Line(points={{26,24},{26,28},{26,32}}, color={0,0,255}));
      connect(ground1.p, transformer.n2) annotation (Line(points={{-40,26},{-40,
              26},{-40,43},{-40,43}}, color={0,0,255}));
      connect(cOut.p, transformer.p2) annotation (Line(points={{8,26},{8,54},{-20,
              54},{-20,53},{-40,53}}, color={0,0,255}));
      connect(cOut.n, transformer.n2) annotation (Line(points={{8,6},{8,-24},{-4,
              -24},{-4,44},{-22,44},{-22,43},{-40,43}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-120,-60},
                {120,100}})),
        Icon(coordinateSystem(extent={{-120,-60},{120,100}},
              preserveAspectRatio=false)),
        experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
<p>OM 23777 OK </p>
</html>"));
    end IsparkOO;

    model IsparkOOmdl
      Real enArc;
      Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-70,-2})));
      Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (Placement(
            transformation(
            extent={{-10,-10},{10,10}},
            rotation=90,
            origin={-70,30})));
      Modelica.Electrical.Analog.Basic.Transformer transformer(
        L1=2.8e-3,
        M=0.252,
        L2=28.0)
        annotation (Placement(transformation(extent={{-30,38},{-10,58}})));
      Modelica.Electrical.Analog.Basic.Ground ground
        annotation (Placement(transformation(extent={{-80,-60},{-60,-40}})));
      Modelica.Electrical.Analog.Basic.Ground ground1
        annotation (Placement(transformation(extent={{-20,6},{0,26}})));
      Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={-30,-22})));
      Modelica.Blocks.Sources.Ramp ramp(
        startTime=0.003,
        height=1000.0,
        duration=0.0002)
        annotation (Placement(transformation(extent={{16,-32},{-4,-12}})));
      Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin1
        annotation (Placement(transformation(extent={{24,48},{44,68}})));
      Modelica.Blocks.Math.Abs abs1
        annotation (Placement(transformation(extent={{56,48},{76,68}})));
      ArcActivation arcActiv
        annotation (Placement(transformation(extent={{72,16},{92,36}})));
      ArcModel arc annotation (Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={46,26})));
      Modelica.Electrical.Analog.Basic.Capacitor cOut(C=15e-12) annotation (
          Placement(transformation(
            extent={{-10,-10},{10,10}},
            rotation=-90,
            origin={24,24})));
    equation
      der(enArc) = arc.p.v*arc.p.i;
      connect(Edc.p, Rbat.p) annotation (Line(
          points={{-70,8},{-70,20}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(transformer.p1, Rbat.n) annotation (Line(
          points={{-30,53},{-70,53},{-70,40}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.p, transformer.n1) annotation (Line(
          points={{-30,-12},{-30,43}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(igbt.n, Edc.n) annotation (Line(
          points={{-30,-32},{-70,-32},{-70,-12}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ground.p, Edc.n) annotation (Line(
          points={{-70,-40},{-70,-12}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(ramp.y, igbt.R) annotation (Line(
          points={{-5,-22},{-19,-22}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(ground1.p, transformer.n2) annotation (Line(points={{-10,26},{-10,
              26},{-10,43}}, color={0,0,255}));
      connect(abs1.u, vInnerPin1.phi) annotation (Line(
          points={{54,58},{45,58}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(vInnerPin1.p, transformer.p2) annotation (Line(
          points={{24,58},{12,58},{12,53},{-10,53}},
          color={0,0,255},
          smooth=Smooth.None));
      connect(arcActiv.u, abs1.y) annotation (Line(
          points={{82.1,37.3},{82.1,58},{77,58}},
          color={0,0,127},
          smooth=Smooth.None));
      connect(arc.p, transformer.p2) annotation (Line(points={{46,36},{46,42},{
              12,42},{12,53},{-10,53}}, color={0,0,255}));
      connect(arc.n, ground1.p) annotation (Line(points={{46,16},{46,10},{12,10},
              {12,26},{-10,26}}, color={0,0,255}));
      connect(arcActiv.y, arc.control) annotation (Line(points={{71,26},{71,26},
              {53.4,26}}, color={255,0,255}));
      connect(cOut.p, transformer.p2) annotation (Line(points={{24,34},{24,42},
              {12,42},{12,53},{-10,53}}, color={0,0,255}));
      connect(cOut.n, ground1.p) annotation (Line(points={{24,14},{24,10},{12,
              10},{12,26},{-10,26}}, color={0,0,255}));
      annotation (
        Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-100,-60},
                {100,80}})),
        Icon(coordinateSystem(extent={{-100,-60},{100,80}}, preserveAspectRatio=
               false)),
        experiment(
          StopTime=0.008,
          StartTime=0,
          Tolerance=0.0001,
          Interval=4e-06),
        __Dymola_experimentSetupOutput,
        Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
<p>OM 23777 OK </p>
</html>"));
    end IsparkOOmdl;

    package OLD
      model sparkGeneration
        Real enArc;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-100,-2})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-100,30})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-60,38},{-40,58}})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-110,-60},{-90,-40}})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{-2,70},{18,90}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-28,6},{-8,26}})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{28,70},{48,90}})));
        Modelica.Electrical.Analog.Ideal.CloserWithArc swArc(
          useHeatPort=false,
          V0=400,
          dVdt=1,
          Vmax=401,
          Goff=0.1e-6,
          Ron=40e3) annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=-90,
              origin={-2,46})));
        Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=
              15e3) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=0,
              origin={80,80})));
        Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-60,-22})));
        Modelica.Blocks.Sources.Ramp ramp(
          startTime=0.003,
          height=1000.0,
          duration=0.0002)
          annotation (Placement(transformation(extent={{-14,-32},{-34,-12}})));
        Modelica.Blocks.Logical.Pre pre1
          annotation (Placement(transformation(extent={{58,36},{38,56}})));
      equation
        der(enArc) = swArc.v*swArc.i;
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-100,8},{-100,20}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-60,53},{-100,53},{-100,50},{-100,50},{-100,40}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{26,80},{19,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(swArc.n, transformer.p2) annotation (Line(
            points={{-2,56},{-26,56},{-26,53},{-40,53}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.n2, swArc.p) annotation (Line(
            points={{-40,43},{-26,43},{-26,36},{-2,36}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground1.p, swArc.p) annotation (Line(
            points={{-18,26},{-18,36},{-2,36}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{-2,80},{-16,80},{-16,56},{-26,56},{-26,53},{-40,53}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(greaterThreshold.u, abs1.y) annotation (Line(
            points={{68,80},{49,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(igbt.p, transformer.n1) annotation (Line(
            points={{-60,-12},{-60,43}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(igbt.n, Edc.n) annotation (Line(
            points={{-60,-32},{-100,-32},{-100,-12}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, Edc.n) annotation (Line(
            points={{-100,-40},{-100,-12}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ramp.y, igbt.R) annotation (Line(
            points={{-35,-22},{-49,-22}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(swArc.control, pre1.y) annotation (Line(
            points={{8,46},{37,46}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(pre1.u, greaterThreshold.y) annotation (Line(
            points={{60,46},{100,46},{100,80},{91,80}},
            color={255,0,255},
            smooth=Smooth.None));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-120,-80},
                  {120,100}}), graphics={Text(
                      extent={{-56,-60},{60,-70}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Manca lo strappo della corrente a fine arco")}),
          Icon(coordinateSystem(extent={{-120,-80},{120,100}},
                preserveAspectRatio=false)),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene al passaggio naturale per 0 della corrente.</p>
<p>***********</p>
<p>Con i dati riportati l&apos;energia della scintilla &egrave; di circa 40 mJ che &egrave; realistico.</p>
<p>L&apos;arco si fa scoccare a soglia di tensione (qui 15 kV.)</p>
<p>OM 23777 OK </p>
</html>"));
      end sparkGeneration;

      model sparkExtinctTimed
        Real enArc;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-100,-2})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-100,30})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-58,34},{-38,54}})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-110,-60},{-90,-40}})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{-4,70},{16,90}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-48,8},{-28,28}})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{26,70},{46,90}})));
        Modelica.Electrical.Analog.Ideal.CloserWithArc closeArc(
          useHeatPort=false,
          dVdt=1e7,
          V0=400,
          Vmax=401,
          Goff=0.1e-6,
          Ron=50e3) annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=-90,
              origin={60,46})));
        Modelica.Electrical.Analog.Sensors.CurrentSensor iArc
          annotation (Placement(transformation(extent={{58,16},{38,36}})));
        Modelica.Blocks.Logical.Timer timer annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={80,-38})));
        Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=
              0.001) annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={42,-38})));
        ArcActivation arcActivate
          annotation (Placement(transformation(extent={{100,36},{120,56}})));
        Modelica.Electrical.Analog.Ideal.OpenerWithArc openArc(
          Goff=0.1e-6,
          V0=1,
          dVdt=1e7,
          Vmax=1e4)
          annotation (Placement(transformation(extent={{4,36},{24,16}})));
        Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-58,-24})));
        Modelica.Blocks.Sources.Ramp ramp(
          startTime=0.003,
          height=1000.0,
          duration=0.0002)
          annotation (Placement(transformation(extent={{-10,-34},{-30,-14}})));
      equation
        der(enArc) = closeArc.v*closeArc.i;
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-100,8},{-100,20}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-58,49},{-80,49},{-80,50},{-100,50},{-100,40}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{24,80},{17,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(iArc.p, closeArc.p) annotation (Line(
            points={{58,26},{60,26},{60,36}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(timer.y, greaterThreshold.u) annotation (Line(
            points={{69,-38},{54,-38}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(abs1.y, arcActivate.u) annotation (Line(
            points={{47,80},{110,80},{110,57.3},{110.1,57.3}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(arcActivate.y, closeArc.control) annotation (Line(
            points={{99,46},{70,46}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(closeArc.n, transformer.p2) annotation (Line(
            points={{60,56},{-38,56},{-38,49}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground1.p, transformer.n2) annotation (Line(
            points={{-38,28},{-38,39}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{-4,80},{-18,80},{-18,56},{-38,56},{-38,49}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(iArc.n, openArc.n) annotation (Line(
            points={{38,26},{24,26}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(openArc.p, transformer.n2) annotation (Line(
            points={{4,26},{-26,26},{-26,34},{-38,34},{-38,39}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(greaterThreshold.y, openArc.control) annotation (Line(
            points={{31,-38},{14,-38},{14,16}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(timer.u, closeArc.control) annotation (Line(
            points={{92,-38},{108,-38},{108,12},{86,12},{86,46},{70,46}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(igbt.p, transformer.n1) annotation (Line(
            points={{-58,-14},{-58,39}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(igbt.n, Edc.n) annotation (Line(
            points={{-58,-34},{-100,-34},{-100,-12}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, Edc.n) annotation (Line(
            points={{-100,-40},{-100,-12}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ramp.y, igbt.R) annotation (Line(
            points={{-31,-24},{-47,-24}},
            color={0,0,127},
            smooth=Smooth.None));
        annotation (
          Icon(coordinateSystem(extent={{-120,-100},{120,100}},
                preserveAspectRatio=false)),
          experiment(StopTime=0.008, __Dymola_NumberOfIntervals=2000),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello l&apos;arco &egrave; modellizzato come una tensione costante in serie con una resistenza.</p>
<p>L&apos;interruzione avviene mediante un secondo interruttore, pilotato per il momento sulla base del tempo. Un modello pi&ugrave; realistico &egrave; invece corrispondente al raggiungimento di una soglia minima di corrente iHold (modelli successivi)</p>
</html>"),Diagram(coordinateSystem(
              extent={{-120,-100},{120,100}},
              preserveAspectRatio=false,
              initialScale=0.1,
              grid={2,2}), graphics={Text(
                      lineColor={0,0,255},
                      extent={{-80,-64},{86,-84}},
                      textString="Accensione Motori ad Accensione Comandata
Aggiunto primo modello per lo strappo della corrente a fine arco con attivazione a tempo
"),Text(              origin={-8,4},
                      lineColor={255,0,0},
                      extent={{-74,-86},{108,-102}},
                      textString=
                  "Tema proposto: openArc come impedenza crescente"),Text(
                      extent={{-106,86},{-48,70}},
                      lineColor={28,108,200},
                      textString="Non fatto 27-11-05")}));
      end sparkExtinctTimed;

      model sparkExtinctCurrent
        Real enArc, enSw;
        Modelica.Electrical.Analog.Sources.ConstantVoltage Edc(V=14)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-106,6})));
        Modelica.Electrical.Analog.Basic.Resistor Rbat(R=2) annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=90,
              origin={-106,38})));
        Modelica.Electrical.Analog.Basic.Transformer transformer(
          L1=2.8e-3,
          M=0.252,
          L2=28.0)
          annotation (Placement(transformation(extent={{-64,42},{-44,62}})));
        Modelica.Electrical.Analog.Basic.Ground ground
          annotation (Placement(transformation(extent={{-116,-52},{-96,-32}})));
        Modelica.Electrical.Analog.Sensors.PotentialSensor vInnerPin
          annotation (Placement(transformation(extent={{-10,70},{10,90}})));
        Modelica.Electrical.Analog.Basic.Ground ground1
          annotation (Placement(transformation(extent={{-54,16},{-34,36}})));
        Modelica.Blocks.Math.Abs abs1
          annotation (Placement(transformation(extent={{20,70},{40,90}})));
        Modelica.Electrical.Analog.Ideal.CloserWithArc closeArc(
          useHeatPort=false,
          dVdt=1e7,
          V0=400,
          Vmax=401,
          Goff=0.1e-6,
          Ron=50e3) annotation (Placement(transformation(
              extent={{10,-10},{-10,10}},
              rotation=-90,
              origin={54,54})));
        Modelica.Electrical.Analog.Sensors.CurrentSensor iArc
          annotation (Placement(transformation(extent={{52,24},{32,44}})));
        ArcActivation arcActivation
          annotation (Placement(transformation(extent={{94,44},{114,64}})));
        Modelica.Electrical.Analog.Ideal.OpenerWithArc openArc(
          Goff=0.1e-6,
          V0=1,
          dVdt=1e7,
          Vmax=1e4)
          annotation (Placement(transformation(extent={{-2,44},{18,24}})));
        Modelica.Blocks.Math.Mean mean(f=10e3) annotation (Placement(
              transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={78,-6})));
        Modelica.Blocks.Logical.LessThreshold lessThreshold(threshold=0.01)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={78,-36})));
        Modelica.Blocks.Logical.And and1 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=180,
              origin={50,-64})));
        Modelica.Blocks.Math.Abs abs2 annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={78,18})));
        Modelica.Blocks.MathBoolean.OnDelay onDelay(delayTime=0.2e-3)
          annotation (Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={110,-36})));
        Modelica.Electrical.Analog.Basic.VariableResistor igbt annotation (
            Placement(transformation(
              extent={{-10,-10},{10,10}},
              rotation=-90,
              origin={-64,-6})));
        Modelica.Blocks.Sources.Ramp ramp(
          startTime=0.003,
          height=1000.0,
          duration=0.0002)
          annotation (Placement(transformation(extent={{-18,-16},{-38,4}})));
      equation
        der(enSw) = igbt.LossPower;
        der(enArc) = closeArc.v*closeArc.i;
        connect(Edc.p, Rbat.p) annotation (Line(
            points={{-106,16},{-106,28}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(transformer.p1, Rbat.n) annotation (Line(
            points={{-64,57},{-86,57},{-86,58},{-106,58},{-106,48}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.u, vInnerPin.phi) annotation (Line(
            points={{18,80},{11,80}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(iArc.p, closeArc.p) annotation (Line(
            points={{52,34},{54,34},{54,44}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(abs1.y, arcActivation.u) annotation (Line(
            points={{41,80},{104,80},{104,65.3},{104.1,65.3}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(arcActivation.y, closeArc.control) annotation (Line(
            points={{93,54},{64,54}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(closeArc.n, transformer.p2) annotation (Line(
            points={{54,64},{-44,64},{-44,57}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground1.p, transformer.n2) annotation (Line(
            points={{-44,36},{-44,47}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(vInnerPin.p, transformer.p2) annotation (Line(
            points={{-10,80},{-24,80},{-24,64},{-44,64},{-44,57}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(iArc.n, openArc.n) annotation (Line(
            points={{32,34},{18,34}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(openArc.p, transformer.n2) annotation (Line(
            points={{-2,34},{-32,34},{-32,42},{-44,42},{-44,47}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(mean.y, lessThreshold.u) annotation (Line(
            points={{78,-17},{78,-24}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(lessThreshold.y, and1.u2) annotation (Line(
            points={{78,-47},{78,-56},{62,-56}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(mean.u, abs2.y) annotation (Line(
            points={{78,6},{78,7}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(abs2.u, iArc.i) annotation (Line(
            points={{78,30},{78,36},{62,36},{62,18},{42,18},{42,24}},
            color={0,0,127},
            smooth=Smooth.None));
        connect(onDelay.u, closeArc.control) annotation (Line(
            points={{110,-22},{110,42},{82,42},{82,54},{64,54}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(onDelay.y, and1.u1) annotation (Line(
            points={{110,-48},{110,-64},{62,-64}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(and1.y, openArc.control) annotation (Line(
            points={{39,-64},{8,-64},{8,24}},
            color={255,0,255},
            smooth=Smooth.None));
        connect(igbt.p, transformer.n1) annotation (Line(
            points={{-64,4},{-64,47}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(igbt.n, Edc.n) annotation (Line(
            points={{-64,-16},{-64,-24},{-106,-24},{-106,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ground.p, Edc.n) annotation (Line(
            points={{-106,-32},{-106,-4}},
            color={0,0,255},
            smooth=Smooth.None));
        connect(ramp.y, igbt.R) annotation (Line(
            points={{-39,-6},{-53,-6}},
            color={0,0,127},
            smooth=Smooth.None));
        annotation (
          Diagram(coordinateSystem(preserveAspectRatio=false, extent={{-120,-80},
                  {120,100}}), graphics={Text(
                      extent={{-114,-60},{2,-70}},
                      lineColor={0,0,255},
                      textString="Accensione Motori ad Accensione Comandata
Aggiunto  modello per lo strappo della corrente a fine arco 
con attivazione a soglia di corrente")}),
          Icon(coordinateSystem(extent={{-120,-80},{120,100}},
                preserveAspectRatio=false)),
          __Dymola_experimentSetupOutput,
          Documentation(info="<html>
<p>In questo modello si sono integrati due modelli di arco: l&apos;innesco dell&apos;arco con tensione costante ins ereie con resistenza, con closeArc; l&apos;estinzione dell&apos;arco con tensione crescente che forza la corrente a 0 con openArc.</p>
<p>La logica di interruzione dell&apos;arco &egrave; realizzata sulla base del raggiungimento di una soglia minima di corrente.</p>
<p>Questa logica &egrave; per&ograve; incapsulata solo parzialmente: manca di mettere in un unico controllore anche la lgcica di openArc.</p>
</html>"),experiment(
            StartTime=0,
            StopTime=0.008,
            Tolerance=0.0001,
            Interval=1.6e-06));
      end sparkExtinctCurrent;
    end OLD;
  end VarResBreakerBased;

  model ArcModel
    Modelica.SIunits.Power LossPower=(p.v - n.v)*p.i;
    parameter Modelica.SIunits.Resistance arcRon=30e3 "arc on resistance";
    parameter Modelica.SIunits.Conductance arcGoff=1e-7 "arc off conductance";
    //  parameter Modelica.SIunits.Resistance diodeRon=1e-8;
    //  parameter Modelica.SIunits.Conductance diodeGoff=1e-8;
    parameter Modelica.SIunits.Voltage arcFem=400 "arc on-state voltage";
    Modelica.Electrical.Analog.Interfaces.PositivePin p
      "Positive pin (potential p.v > n.v for positive voltage drop v)"
      annotation (Placement(transformation(extent={{-110,-10},{-90,10}}),
          iconTransformation(extent={{-110,-10},{-90,10}})));
    Modelica.Electrical.Analog.Interfaces.NegativePin n "Negative pin"
      annotation (Placement(transformation(extent={{110,-10},{90,10}}),
          iconTransformation(extent={{110,-10},{90,10}})));
    Modelica.Blocks.Interfaces.BooleanInput control
      "true => p--n connected, false => switch open" annotation (Placement(
          transformation(
          origin={0,74},
          extent={{-10,-10},{10,10}},
          rotation=270), iconTransformation(
          extent={{-10,-10},{10,10}},
          rotation=270,
          origin={0,74})));
    Modelica.Electrical.Analog.Sources.ConstantVoltage sparkFem(V=arcFem)
      annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=180,
          origin={42,0})));
    Modelica.Electrical.Analog.Ideal.IdealDiode diode(Ron=1e-6*arcRon, Goff=
          1e-6*arcGoff) annotation (Placement(transformation(
          extent={{10,-10},{-10,10}},
          rotation=0,
          origin={6,0})));
    Modelica.Electrical.Analog.Ideal.IdealClosingSwitch clArc(Ron=arcRon, Goff=
          arcGoff) annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={-30,0})));
  equation
    connect(n, sparkFem.p)
      annotation (Line(points={{100,0},{52,0}}, color={0,0,255}));
    connect(sparkFem.n, diode.p) annotation (Line(points={{32,1.22125e-015},{32,
            1.22125e-015},{32,0},{16,0}}, color={0,0,255}));
    connect(diode.n, clArc.n)
      annotation (Line(points={{-4,0},{-20,0}}, color={0,0,255}));
    connect(control, clArc.control) annotation (Line(points={{0,74},{0,40},{-30,
            40},{-30,7}}, color={255,0,255}));
    connect(clArc.p, p)
      annotation (Line(points={{-40,0},{-100,0}}, color={0,0,255}));
    annotation (Icon(coordinateSystem(preserveAspectRatio=false), graphics={
          Ellipse(
            extent={{-42,12},{-24,-12}},
            lineColor={0,0,0},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid),
          Line(points={{-92,0},{-46,0}}),
          Line(points={{44,0},{94,0}}),
          Text(
            extent={{-146,-46},{154,-86}},
            textString="%name",
            lineColor={0,0,255}),
          Line(points={{28,-2},{-8,14},{4,-8},{-28,0}}, color={255,0,0}),
          Line(points={{0,88},{0,24}}, color={255,85,255}),
          Rectangle(
            extent={{24,29},{44,-31}},
            lineColor={95,95,95},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-74,12},{-32,-12}},
            lineColor={95,95,95},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid),
          Rectangle(
            extent={{-10,30},{10,-30}},
            lineColor={95,95,95},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid,
            origin={70,35},
            rotation=-90),
          Ellipse(
            extent={{25,43.5},{54,17}},
            lineColor={95,95,95},
            fillColor={95,95,95},
            fillPattern=FillPattern.Solid)}), Diagram(coordinateSystem(
            preserveAspectRatio=false)));
  end ArcModel;

  model ArcActivation
    Modelica.Blocks.Logical.GreaterThreshold greaterThreshold(threshold=threas)
      annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=0,
          origin={30,50})));
    Modelica.Blocks.Logical.Or or1 annotation (Placement(transformation(
          extent={{-10,-10},{10,10}},
          rotation=-90,
          origin={74,16})));
    Modelica.Blocks.Logical.Pre pre1 annotation (Placement(transformation(
          extent={{10,-10},{-10,10}},
          rotation=-90,
          origin={50,16})));
    Modelica.Blocks.Logical.Pre pre2 "Just to prevent an algebraic loop"
      annotation (Placement(transformation(
          extent={{10,-10},{-10,10}},
          rotation=0,
          origin={2,-20})));
    Modelica.Blocks.Interfaces.BooleanOutput y annotation (Placement(
          transformation(rotation=0, extent={{-100,-10},{-120,10}}),
          iconTransformation(extent={{-100,-10},{-120,10}})));
    Modelica.Blocks.Interfaces.RealInput u annotation (Placement(transformation(
          rotation=-90,
          extent={{-13,-13},{13,13}},
          origin={1,113})));
    parameter Real threas=15e3 "Voltage threasold";
  equation
    connect(or1.u1, greaterThreshold.y) annotation (Line(
        points={{74,28},{74,50},{41,50}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(pre1.y, or1.u2) annotation (Line(
        points={{50,27},{50,34},{66,34},{66,28}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(pre1.u, or1.y) annotation (Line(
        points={{50,4},{50,-6},{74,-6},{74,5}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(pre2.u, or1.y) annotation (Line(
        points={{14,-20},{74,-20},{74,5}},
        color={255,0,255},
        smooth=Smooth.None));
    connect(u, greaterThreshold.u)
      annotation (Line(points={{1,113},{1,50},{18,50}}, color={0,0,127}));
    connect(pre2.y, y) annotation (Line(
        points={{-9,-20},{-78,-20},{-78,0},{-110,0}},
        color={255,0,255},
        smooth=Smooth.None));
    annotation (Diagram(coordinateSystem(extent={{-100,-100},{100,100}},
            preserveAspectRatio=false), graphics), Icon(coordinateSystem(extent=
             {{-100,-100},{100,100}}, preserveAspectRatio=false), graphics));
  end ArcActivation;
  annotation (uses(Modelica(version="3.2.2")));
end sparkPkg16;
