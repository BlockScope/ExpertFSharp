open System.Windows.Forms

let form = new Form(Text = "Hello World WinForms")
let button = new Button(Text = "Click Me!", Dock = DockStyle.Fill)
//val form : Form = System.Windows.Forms.Form, Text: Hello World WinForms
//val button : Button = System.Windows.Forms.Button, Text: Click Me!

button.Click.Add(fun _ -> MessageBox.Show("Hello, World!", "Hey!") |> ignore)
form.Controls.Add(button)
form.Show()

Application.Run(form)
// The above with throw if run from F# interactive with ...
// System.InvalidOperationException: Starting a second message loop on a single thread is not a valid operation. Use Form.ShowDialog instead.

Application.EnableVisualStyles()

let form = new Form(Text = "Explicit Event Loop")
form.Show()
while form.Created do
    // Perform some task
    Application.DoEvents()

open System
open System.Drawing
open System.Windows.Forms

[<STAThread>]
do Application.EnableVisualStyles()

let statusProgress =
    new ToolStripProgressBar(Size = new Size(200, 16),
                             Style = ProgressBarStyle.Marquee,
                             Visible = false)
let status = new StatusStrip(Dock = DockStyle.Bottom)
status.Items.Add(statusProgress) |> ignore
//val statusProgress : ToolStripProgressBar =
//  System.Windows.Forms.ToolStripProgressBar
//val status : StatusStrip = System.Windows.Forms.StatusStrip, Name: , Items: 1
//val it : unit = ()

let toolbar = new ToolStrip(Dock = DockStyle.Top)
let address = new ToolStripTextBox(Size = new Size(400, 25))
let browser = new WebBrowser(Dock = DockStyle.Fill)
let go = new ToolStripButton(DisplayStyle = ToolStripItemDisplayStyle.Text,
                             Text = "Go")
address.KeyPress.Add(fun arg ->
    if (arg.KeyChar = '\r') then browser.Url <- new Uri(address.Text))
go.Click.Add(fun arg -> browser.Url <- new Uri(address.Text))
toolbar.Items.Add(new ToolStripLabel("Address:")) |> ignore
toolbar.Items.Add(address) |> ignore
toolbar.Items.Add(go) |>ignore

browser.Navigating.Add(fun args ->
    statusProgress.Visible <- true)
browser.DocumentCompleted.Add(fun args ->
    statusProgress.Visible <- false;
    address.Text <- browser.Url.AbsoluteUri)

let form = new Form(Text = "Web Browser", Size = new Size(800, 600))
form.Controls.Add(browser)
form.Controls.Add(toolbar)
form.Controls.Add(status)
form.PerformLayout()
form.Show()

//val toolbar : ToolStrip = System.Windows.Forms.ToolStrip, Name: , Items: 0
//val address : ToolStripTextBox = System.Windows.Forms.ToolStripTextBox
//val browser : WebBrowser = System.Windows.Forms.WebBrowser
//val go : ToolStripButton = Go
//val form : Form = System.Windows.Forms.Form, Text: Web Browser


Application.Run(form)

open System
open System.Drawing
open System.Windows.Forms

Application.EnableVisualStyles()
Application.SetCompatibleTextRenderingDefault(false)
// The above call with throw if run from F# interactive with ...
// System.InvalidOperationException: SetCompatibleTextRenderingDefault must be called before the first IWin32Window object is created in the application.

let form = new Form(Text = "Curves")
let cpt = [|Point(20, 60); Point(40, 50); Point(130, 60); Point(200, 200)|]
let mutable movingPoint = -1

let newMenu (s : string) = new ToolStripMenuItem(s,Checked = true,CheckOnClick = true)
let menuBezier = newMenu "Show &Bézier"
let menuCanonical = newMenu "Show &Canonical spline"
let menuControlPoints = newMenu "Show control &points"

let scrollbar = new VScrollBar(Dock = DockStyle.Right, LargeChange = 2, Maximum = 10)

let drawPoint (g : Graphics) (p : Point) =
    g.DrawEllipse(Pens.Red, p.X - 2, p.Y - 2, 4, 4)

let paint (g : Graphics) =
    if (menuBezier.Checked) then
        g.DrawLine(Pens.Red, cpt.[0], cpt.[1])
        g.DrawLine(Pens.Red, cpt.[2], cpt.[3])
        g.DrawBeziers(Pens.Black, cpt)
    if (menuCanonical.Checked) then
        g.DrawCurve(Pens.Blue, cpt, float32 scrollbar.Value)
    if (menuControlPoints.Checked) then
        for i = 0 to cpt.Length - 1 do
            drawPoint g cpt.[i]

let isClose (p : Point) (l : Point) =
    let dx = p.X - l.X
    let dy = p.Y - l.Y
    (dx * dx + dy * dy) < 6

let mouseDown (p : Point) =
    try
      let idx = cpt |> Array.findIndex (isClose p)
      movingPoint <- idx
    with _ -> ()

let mouseMove (p : Point) =
    if (movingPoint <> -1) then
        cpt.[movingPoint] <- p
        form.Invalidate()

let setupMenu () =
    let menu = new MenuStrip()
    let fileMenuItem = new ToolStripMenuItem("&File")
    let settMenuItem = new ToolStripMenuItem("&Settings")
    let exitMenuItem = new ToolStripMenuItem("&Exit")
    menu.Items.Add(fileMenuItem) |> ignore
    menu.Items.Add(settMenuItem) |> ignore
    fileMenuItem.DropDownItems.Add(exitMenuItem) |> ignore
    settMenuItem.DropDownItems.Add(menuBezier) |> ignore
    settMenuItem.DropDownItems.Add(menuCanonical) |> ignore
    settMenuItem.DropDownItems.Add(menuControlPoints) |> ignore
    exitMenuItem.Click.Add(fun _ -> form.Close ())
    menuBezier.Click.Add(fun _ -> form.Invalidate())
    menuCanonical.Click.Add(fun _ -> form.Invalidate())
    menuControlPoints.Click.Add(fun _ -> form.Invalidate())
    menu

scrollbar.ValueChanged.Add(fun _ -> form.Invalidate())
form.Controls.Add(scrollbar)
form.MainMenuStrip <- setupMenu()
form.Controls.Add(form.MainMenuStrip)
form.Paint.Add(fun e -> paint(e.Graphics))
form.MouseDown.Add(fun e -> mouseDown(e.Location))
form.MouseMove.Add(fun e -> mouseMove(e.Location))
form.MouseUp.Add(fun e -> movingPoint <- -1)
form.Show()
//val form : Form = System.Windows.Forms.Form, Text: Curves
//val cpt : Point [] = [|{X=20,Y=60}; {X=40,Y=50}; {X=130,Y=60}; {X=200,Y=200}|]
//val mutable movingPoint : int = -1
//val newMenu : s:string -> ToolStripMenuItem
//val menuBezier : ToolStripMenuItem = Show &Bézier
//val menuCanonical : ToolStripMenuItem = Show &Canonical spline
//val menuControlPoints : ToolStripMenuItem = Show control &points
//val scrollbar : VScrollBar =
//  System.Windows.Forms.VScrollBar, Minimum: 0, Maximum: 10, Value: 0
//val drawPoint : g:Graphics -> p:Point -> unit
//val paint : g:Graphics -> unit
//val isClose : p:Point -> l:Point -> bool
//val mouseDown : p:Point -> unit
//val mouseMove : p:Point -> unit
//val setupMenu : unit -> MenuStrip

[<STAThread>]
do Application.Run(form)

#r @".\15Visual\ExpertFSharp.UserControls\bin\Debug\ExpertFSharp3.UserControls.dll"
open ExpertFSharp3.UserControls

let form = new Form(Visible = true)
let c = new OwnerDrawButton(Text = "Hello button")

c.Click.Add(fun _ -> MessageBox.Show("Clicked!") |> ignore)
form.Controls.Add(c)

//c.AddSample(t, v)

let timer = new Timer(Interval = 200)

let rnd = new Random()
let time = ref 0
let data = timer.Tick |> Observable.map(fun _ ->
    incr time
    let v = 48.0 + 2.0 * rnd.NextDouble()
    (!time, v))
//val timer : Timer = System.Windows.Forms.Timer, Interval: 200
//val rnd : Random
//val time : int ref = {contents = 0;}
//val data : IObservable<int * float>

#r @".\15Visual\packages\MSDN.FSharpChart.dll.0.60\lib\MSDN.FSharpChart.dll"
open MSDN.FSharp.Charting
FSharpChart.Line(data, MaxPoints = 20)
//val it : ChartTypes.LineChart =
//  MSDN.FSharp.Charting.ChartTypes+LineChart
//    {Area = ChartArea; ... }

form.Controls.Add(FSharpChart.Line(data, MaxPoints = 20))

type Sample = {Time : int64; Value : float32}
//type Sample =
//  {Time: int64;
//   Value: float32;}

open System

type Sample = {Time : int64; Value : float32}

type DataSamples() =
    let data = new ResizeArray<Sample>()
    let mutable count = 0
    let mutable lastTime = 0L

    member x.Last = {Time = lastTime; Value = data.[data.Count - 1].Value}

    member x.AddSample(t, v) =
        let s = {Time = t; Value = v}
        let last = if (data.Count = 0) then s else x.Last

        count <- count + 1
        lastTime <- max last.Time s.Time
        if data.Count = 0 then data.Add(s)

        elif last.Time < s.Time && last.Value <> s.Value then
            if data.[data.Count - 1].Time <> last.Time then data.Add(last)
            data.Add(s)

    member x.Count = count

    // The model is continuous: missing samples are obtained by interpolation
    member x.GetValue(time : int64) =

        // Find the relevant point via a binary search
        let rec search (lo, hi) =
            let mid = (lo + hi) / 2
            if hi - lo <= 1 then (lo, hi)
            elif data.[mid].Time = time then (mid, mid)
            elif data.[mid].Time < time then search (mid, hi)
            else search (lo, mid)

        if (data.Count = 0) then failwith "No data samples"

        if (lastTime < time) then failwith "Wrong time!"

        let lo, hi = search (0, data.Count - 1)

        if (data.[lo].Time = time || hi = lo) then data.[lo].Value
        elif (data.[hi].Time = time) then data.[hi].Value
        else
            // interpolate
            let p = if data.[hi].Time < time then hi else lo
            let next = data.[min (p+1) (data.Count-1)]
            let curr = data.[p]
            let spant = next.Time - curr.Time
            let spanv = next.Value - curr.Value
            curr.Value + float32(time-curr.Time) *(spanv / float32 spant)

    // This method finds the minimum and the maximum values given
    // a sampling frequency and an interval of time
    member x.FindMinMax(sampleFreq : int64, start : int64, finish : int64,
                        minval : float32, maxval : float32) =


        if (data.Count = 0) then (minval, maxval) else
        let start = max start 0L
        let finish = min finish lastTime

        let minv, maxv =
            seq {start .. sampleFreq .. finish}
            |> Seq.map x.GetValue
            |> Seq.fold (fun (minv, maxv) v -> (min v minv, max v maxv))
                        (minval, maxval)

        if (minv = maxv) then
            let delta = if (minv = 0.0f) then 0.01f else 0.01f * abs minv
            (minv - delta, maxv + delta)
        else (minv, maxv)
//type DataSamples =
//  class
//    new : unit -> DataSamples
//    member AddSample : t:int64 * v:float32 -> unit
//    member
//      FindMinMax : sampleFreq:int64 * start:int64 * finish:int64 *
//                   minval:float32 * maxval:float32 -> float32 * float32
//    member GetValue : time:int64 -> float32
//    member Count : int
//    member Last : Sample
//  end

let mutable axisColor : Color = Color.White
[<Category("Graph Style"); Browsable(true)>]
member x.AxisColor
    with get() = x.axisColor
    and set(v : Color) = x.axisColor <- v; x.Invalidate()

open System
open System.Drawing
open System.Drawing.Drawing2D
open System.Windows.Forms
open System.ComponentModel

type GraphControl() as x  =
    inherit UserControl()

    let data = new DataSamples()
    let mutable minVisibleValue = Single.MaxValue
    let mutable maxVisibleValue = Single.MinValue
    let mutable absMax = Single.MinValue
    let mutable absMin = Single.MaxValue
    let mutable lastMin = minVisibleValue
    let mutable lastMax = maxVisibleValue
    let mutable axisColor = Color.White
    let mutable beginColor = Color.Red
    let mutable verticalLabelFormat = "{0:F2}"
    let mutable startTime = 0L
    let mutable visibleSamples = 10
    let mutable initView = startTime - int64(visibleSamples)
    let mutable verticalLines = 0
    let mutable timeScale = 10000000 // In 100-nanoseconds
    let mutable timeFormat = "{0:T}"

    let rightBottomMargin = Size(10, 10)
    let leftTopMargin = Size(10, 10)

    do
        x.SetStyle(ControlStyles.AllPaintingInWmPaint, true)
        x.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
        base.BackColor <- Color.DarkBlue

    [<Category("Graph Style"); Browsable(true)>]
    member x.AxisColor
        with get() = axisColor
        and set(v : Color) = axisColor <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.BeginColor
        with get() = beginColor
        and set(v : Color) = beginColor <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.MinVisibleValue
        with get() = minVisibleValue
        and set(v : float32) = minVisibleValue <- v; lastMin <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.MaxVisibleValue
        with get() = maxVisibleValue
        and set(v : float32) = maxVisibleValue <- v; lastMax <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.VerticalLines
        with get() = verticalLines
        and set(v : int) = verticalLines <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.GraphBackColor
        with get() = x.BackColor
        and set(v : Color) = x.BackColor <- v

    [<Category("Graph Style"); Browsable(true)>]
    member x.LineColor
        with get() = x.ForeColor
        and set(v : Color) = x.ForeColor <- v

    [<Category("Graph Style"); Browsable(true)>]
    member x.VerticalLabelFormat
        with get() = verticalLabelFormat
        and set(v : string) = verticalLabelFormat <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.StartTime
        with get() = startTime
        and set(v : int64) = startTime <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.Title
        with get() = x.Text
        and set(v : string) = x.Text <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.VisibleSamples
        with get() = visibleSamples
        and set(v : int) =
            visibleSamples <- v;
            initView <- startTime - int64(visibleSamples);
            x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.TimeScale
        with get() = timeScale
        and set(v : int) = timeScale <- v; x.Invalidate()

    [<Category("Graph Style"); Browsable(true)>]
    member x.TimeFormat
        with get() = timeFormat
        and set(v : string) = timeFormat <- v; x.Invalidate()

   // ... Further portions of this class shown further below

    override x.OnMouseWheel (e : MouseEventArgs) =
        base.OnMouseWheel(e)
        x.Zoom(e.Delta)

    override x.OnSizeChanged (e : EventArgs) =
        base.OnSizeChanged(e)
        x.Invalidate()

    member x.Zoom (amount : int) =
        let newVisibleSamples = max 5 (visibleSamples + amount)
        if (initView - startTime < 0L) then
            let e = initView + int64(visibleSamples)
            initView <- startTime - int64(newVisibleSamples) + e
            visibleSamples <- newVisibleSamples
            x.Invalidate()

    member x.AddSample (time : int64, value : float32) =
        if (value < absMin) then absMin <- value
        if (value > absMax) then absMax <- value
        if (data.Count > 0) then
            initView <- initView + time - data.Last.Time
        data.AddSample(time, value)
        x.Invalidate()

    member x.GetTime (time : int64) =
        DateTime(max 0L time * int64(timeScale))

// The following code is in the used in the implementation of OnPaint below ...
//g.TranslateTransform(float32(plotBox.Left), float32(x.Height - plotBox.Top))
//g.ScaleTransform(1.0f, -1.0f)

    override x.OnPaint (e : PaintEventArgs) =
        let g = e.Graphics

        // A helper function to size up strings
        let measurestring s = g.MeasureString(s, x.Font)

        // Work out the size of the box to show the values
        let valBox =
            let minbox = measurestring (String.Format(verticalLabelFormat, lastMin))
            let maxbox = measurestring (String.Format(verticalLabelFormat, lastMax))
            let vbw = max minbox.Width maxbox.Width
            let vbh = max minbox.Height maxbox.Height
            SizeF(vbw, vbh)

        // Work out the size of the box to show the times
        let timeBox =
            let lasttime = x.GetTime(initView + int64(visibleSamples))
            let timelbl = String.Format(timeFormat, lasttime)
            measurestring timelbl

        // Work out the plot area for the graph
        let plotBox =
            let ltm = leftTopMargin
            let rbm = rightBottomMargin

            let ltm, rbm =
                let ltm = Size(width = max ltm.Width (int(valBox.Width) + 5),
                               height = max ltm.Height (int(valBox.Height / 2.0f) + 2))
                let rbm = Size(width = rightBottomMargin.Width,
                               height = max rbm.Height (int(timeBox.Height) + 5))
                ltm, rbm

            // Since we invert y axis use Top instead of Bottom and vice versa
            Rectangle(ltm.Width, rbm.Height,
                      x.Width - ltm.Width - rbm.Width,
                      x.Height - ltm.Height - rbm.Height)
        // The time interval per visible sample
        let timePerUnit =
            let samplew = float32(visibleSamples) / float32(plotBox.Width)
            max 1.0f samplew

        // The pixel interval per visible sample
        let pixelsPerUnit =
            let pixelspan = float32(plotBox.Width) / float32(visibleSamples)
            max 1.0f pixelspan

        // Compute the range we need to plot
        let (lo, hi) = data.FindMinMax(int64(timePerUnit),
                                       initView,
                                       initView + int64(visibleSamples),
                                       minVisibleValue,
                                       maxVisibleValue)

        // Save the range to help with computing sizes next time around
        lastMin <- lo; lastMax <- hi

        // We use these graphical resources during plotting
        use linePen = new Pen(x.ForeColor)
        use axisPen = new Pen(axisColor)
        use beginPen = new Pen(beginColor)
        use gridPen = new Pen(Color.FromArgb(127, axisColor),
                              DashStyle = DashStyle.Dash)
        use fontColor = new SolidBrush(axisColor)

        // Draw the title
        if (x.Text <> null && x.Text <> String.Empty) then

            let sz = measurestring x.Text
            let mw = (float32(plotBox.Width) - sz.Width) / 2.0f
            let tm = float32(plotBox.Bottom - plotBox.Height)

            let p = PointF(float32(plotBox.Left) + mw, tm)
            g.DrawString(x.Text, x.Font, new SolidBrush(x.ForeColor), p)

        // Draw the labels
        let nly = int((float32(plotBox.Height) / valBox.Height) / 3.0f)
        let nlx = int((float32(plotBox.Width) / timeBox.Width) / 3.0f)
        let pxly = plotBox.Height / max nly 1
        let pxlx = plotBox.Width / max nlx 1
        let dvy = (hi - lo) / float32(nly)
        let dvx = float32(visibleSamples) / float32(nlx)
        let drawString (s : string) (xp : float32) (yp : float32) =
            g.DrawString(s, x.Font, fontColor, xp, yp)

        // Draw the value (y) labels
        for i = 0 to nly do
            let liney = i * pxly + int(valBox.Height / 2.0f) + 2
            let lblfmt = verticalLabelFormat
            let posy = float32(x.Height - plotBox.Top - i * pxly)
            let label = String.Format(lblfmt, float32(i) * dvy + lo)
            drawString label (float32(plotBox.Left) - valBox.Width)
                             (posy - valBox.Height / 2.0f)

            if (i = 0 || ((i > 0) && (i < nly))) then
                g.DrawLine(gridPen, plotBox.Left, liney, plotBox.Right, liney)

        // Draw the time (x) labels
        for i = 0 to nlx do
            let linex = i * pxlx + int(timeBox.Width / 2.0f) + 2
            let time = int64(float32(i) * dvx + float32(initView))
            let label = String.Format(timeFormat, x.GetTime(time))

            if (time > 0L) then
                drawString label
                    (float32(plotBox.Left + i * pxlx) + timeBox.Width / 2.0f)
                    (float32(x.Height - plotBox.Top + 2))

        // Set a transform on the graphics state to make drawing in the
        // plotBox simpler
        g.TranslateTransform(float32(plotBox.Left),
                              float32(x.Height - plotBox.Top));
        g.ScaleTransform(1.0f, -1.0f);

        // Draw the plotBox of the plot area
        g.DrawLine(axisPen, 0, 0, 0, plotBox.Height)
        g.DrawLine(axisPen, 0, 0, plotBox.Width, 0)
        g.DrawLine(axisPen, plotBox.Width, 0, plotBox.Width, plotBox.Height)
        g.DrawLine(axisPen, 0, plotBox.Height, plotBox.Width, plotBox.Height)


        // Draw the vertical lines in the plotBox
        let px = plotBox.Width / (verticalLines + 1)
        for i = 1 to verticalLines do
            g.DrawLine(gridPen, i * px, 0, i * px, plotBox.Height)

        // Draw the 'begin' marker that shows where data begins
        if (initView - startTime <= 0L) then
            let off = float32(Math.Abs(x.StartTime - initView))
            let sx = int((off / timePerUnit) * pixelsPerUnit)
            g.DrawLine(beginPen, sx, 0, sx, plotBox.Height)

        // Draw the 'zero' horizontal line if it's visible
        if (hi <> lo && lo < 0.0f) then
            let sy = int((float32(plotBox.Height) / (hi - lo)) * (0.0f - lo))
            g.DrawLine(axisPen, 0, sy, plotBox.Width, sy)

        // Draw the visible data samples
        let rec drawSamples i pos =
            if (i < (float32(plotBox.Width) / pixelsPerUnit) &&
                pos <= (initView + int64 visibleSamples - int64 timePerUnit)) then

                if (pos >= 0L) then
                    let dh = float32(plotBox.Height) / (hi - lo)
                    let sx = int(pixelsPerUnit * i)
                    let dx = int(pixelsPerUnit * (i + 1.0f))
                    let sy = int(dh * (data.GetValue(pos) - lo))
                    let dy = int(dh * (data.GetValue(pos + int64 timePerUnit) - lo))
                    g.DrawLine(linePen, sx, sy, dx, dy);

                drawSamples (i + 1.0f) (pos + int64 timePerUnit)

        drawSamples 0.0f initView
//type GraphControl =
//  class
//    inherit UserControl
//    new : unit -> GraphControl
//    member AddSample : time:int64 * value:float32 -> unit
//    member GetTime : time:int64 -> DateTime
//    override OnMouseWheel : e:MouseEventArgs -> unit
//    override OnPaint : e:PaintEventArgs -> unit
//    override OnSizeChanged : e:EventArgs -> unit
//    member Zoom : amount:int -> unit
//    member AxisColor : Color
//    member BeginColor : Color
//    member GraphBackColor : Color
//    member LineColor : Color
//    member MaxVisibleValue : float32
//    member MinVisibleValue : float32
//    member StartTime : int64
//    member TimeFormat : string
//    member TimeScale : int
//    member Title : string
//    member VerticalLabelFormat : string
//    member VerticalLines : int
//    member VisibleSamples : int
//    member AxisColor : Color with set
//    member BeginColor : Color with set
//    member GraphBackColor : Color with set
//    member LineColor : Color with set
//    member MaxVisibleValue : float32 with set
//    member MinVisibleValue : float32 with set
//    member StartTime : int64 with set
//    member TimeFormat : string with set
//    member TimeScale : int with set
//    member Title : string with set
//    member VerticalLabelFormat : string with set
//    member VerticalLines : int with set
//    member VisibleSamples : int with set
//  end
let form = new Form(Text = "Chart test", Size = Size(800, 600), Visible = true, TopMost = true)
let graph = new GraphControl(VisibleSamples = 60, Dock = DockStyle.Fill)
let properties = new PropertyGrid(Dock = DockStyle.Fill)
let timer = new Timer(Interval = 200)
let container = new SplitContainer(Dock = DockStyle.Fill, SplitterDistance = 350)
//val form : Form = System.Windows.Forms.Form, Text: Chart test
//val graph : GraphControl = FSI_0035+GraphControl
//val properties : PropertyGrid = System.Windows.Forms.PropertyGrid
//val timer : Timer = System.Windows.Forms.Timer, Interval: 200
//val container : SplitContainer = System.Windows.Forms.SplitContainer

// We use a split container to divide the area into two parts
container.Panel1.Controls.Add(graph)
container.Panel2.Controls.Add(properties)

// Configure the property grid to display only properties in the
// category "Graph Style"
properties.SelectedObject <- graph
let graphStyleCat = (CategoryAttribute("Graph Style") :> Attribute)
properties.BrowsableAttributes <- AttributeCollection([|graphStyleCat|])
form.Controls.Add(container)
let rnd = new Random()
let time = ref 0
// A timer is used to simulate incoming data
timer.Tick.Add(fun _ ->
    incr time
    let v = 48.0 + 2.0 * rnd.NextDouble()
    graph.AddSample(int64(!time), float32(v)))
timer.Start()
form.Disposed.Add(fun _ -> timer.Stop())
//val graphStyleCat : Attribute
//val rnd : Random
//val time : int ref = {contents = 0;}

c = a + bi

c1 + c2 = (a1 + a2) + i(b1 + b2)
c1 • c2 = (a1 • a2 – b1 • b2) + i(a1 • b2 + a2 • b1)

open System.Numerics

let sqrMod (x : Complex) = x.Real * x.Real + x.Imaginary * x.Imaginary
let rec mandel maxit (z : Complex) (c : Complex) count =
    if (sqrMod(z) < 4.0) &&  (count < maxit) then
        mandel maxit ((z * z) + c) c (count + 1)
    else count
//val sqrMod : x:Numerics.Complex -> float
//val mandel :
//  maxit:int -> z:Numerics.Complex -> c:Numerics.Complex -> count:int -> int

let RGBtoHSV (r, g, b) =
    let (m : float) = min r (min g b)
    let (M : float) = max r (max g b)
    let delta = M - m
    let posh (h : float) = if h < 0.0 then h + 360.0 else h
    let deltaf (f : float) (s : float) = (f - s) / delta
    if M = 0.0 then (-1.0, 0.0, M) else
        let s = (M - m) / M
        if r = M then (posh(60.0 * (deltaf g b)), s, M)
        elif g = M then (posh(60.0 * (2.0 + (deltaf b r))), s, M)
        else (posh(60.0 * (4.0 + (deltaf r g))), s, M)

let HSVtoRGB (h, s, v) =
    if s = 0.0 then (v, v, v) else
    let hs = h / 60.0
    let i = floor (hs)
    let f = hs - i
    let p = v * ( 1.0 - s )
    let q = v * ( 1.0 - s * f )
    let t = v * ( 1.0 - s * ( 1.0 - f ))
    match int i with
      | 0 -> (v, t, p)
      | 1 -> (q, v, p)
      | 2 -> (p, v, t)
      | 3 -> (p, q, v)
      | 4 -> (t, p, v)
      | _ -> (v, p, q)
//val RGBtoHSV : r:float * g:float * b:float -> float * float * float
//val HSVtoRGB : h:float * s:float * v:float -> float * float * float

open System.Drawing
open System.Windows.Forms

let makeColor (r, g, b) =
    Color.FromArgb(int32(r * 255.0), int32(g * 255.0), int32(b * 255.0))

let defaultColor i = makeColor(HSVtoRGB(360.0 * (float i / 250.0), 1.0, 1.0))

let coloring =
    [|
        defaultColor;
        (fun i -> Color.FromArgb(i, i, i));
        (fun i -> Color.FromArgb(i, 0, 0));
        (fun i -> Color.FromArgb(0, i, 0));
        (fun i -> Color.FromArgb(0, 0, i));
        (fun i -> Color.FromArgb(i, i, 0));
        (fun i -> Color.FromArgb(i, 250 - i, 0));
        (fun i -> Color.FromArgb(250 - i, i, i));
        (fun i -> if i % 2 = 0 then Color.White else Color.Black);
        (fun i -> Color.FromArgb(250 - i, 250 - i, 250 - i))
    |]

let createPalette c =
    Array.init 253 (function
        | 250 -> Color.Black
        | 251 -> Color.White
        | 252 -> Color.LightGray
        | i ->   c i)

let mutable palette = createPalette coloring.[0]

let pickColor maxit it =
    palette.[int(250.0 * float it / float maxit)]
//val makeColor : r:float * g:float * b:float -> Color
//val defaultColor : i:int -> Color
//val coloring : (int -> Color) [] =
//  [|<fun:coloring@710>; <fun:coloring@711-1>; ...|]
//val createPalette : c:(int -> Color) -> Color []
//val mutable palette : Color [] =
//  [|Color [A=255, R=255, G=0, B=0]; Color [A=255, R=255, G=6, B=0]; ...|]
//val pickColor : maxit:int -> it:int -> Color

let mutable bmpw = form.Width
let mutable bmph = form.Height
let mutable menuIterations = 150

let iterations (tlx, tly) (brx, bry) =
    menuIterations
//val mutable bmpw : int = 1019
//val mutable bmph : int = 600
//val mutable menuIterations : int = 150
//val iterations : tlx:'a * tly:'b -> brx:'c * bry:'d -> int

let run filler (form : #Form) (bitmap : Bitmap) (tlx, tly) (brx, bry) =
    let dx = (brx - tlx) / float bmpw
    let dy = (tly - bry) / float bmph
    let maxit = iterations (tlx, tly) (brx, bry)
    let x = 0
    let y = 0
    let transform x y = new Complex(tlx + (float x) * dx, tly - (float y) * dy)
    form.Invoke(new MethodInvoker(fun () ->
        form.Text <- sprintf "Mandelbrot set [it: %d] (%f, %f) -> (%f, %f)"
                     maxit tlx tly brx bry
    )) |> ignore
    filler maxit transform
    timer.Enabled <- false
//val run :
//  filler:(int -> (int -> int -> Complex) -> unit) ->
//    form:#Form ->
//      bitmap:Bitmap -> tlx:float * tly:float -> brx:float * bry:float -> unit

let mutable bitmap = new Bitmap(form.Width, form.Height)
//val mutable bitmap : Bitmap

let linearFill (bw : int) (bh : int) maxit map =
    for y = 0 to bh - 1 do
        for x = 0 to bw - 1 do
            let c = mandel maxit Complex.Zero (map x y) 0
            lock bitmap (fun () -> bitmap.SetPixel(x, y, pickColor maxit c))
//val linearFill :
//  bw:int -> bh:int -> maxit:int -> map:(int -> int -> Complex) -> unit

let blockFill (bw : int) (bh : int) maxit map =
    let rec fillBlock first sz x y =
        if x < bw then
            let c = mandel maxit Complex.Zero (map x y) 0
            lock bitmap (fun () ->
                let g = Graphics.FromImage(bitmap)
                g.FillRectangle(new SolidBrush(pickColor maxit c), x, y, sz, sz)
                g.Dispose())
            fillBlock first sz (if first || ((y / sz) % 2 = 1) then x + sz else x + 2 * sz) y
        elif y < bh then
            fillBlock first sz (if first || ((y / sz) % 2 = 0) then 0 else sz) (y + sz)
        elif sz > 1 then
            fillBlock false (sz / 2) (sz / 2) 0

    fillBlock true 64 0 0
//val blockFill :
//  bw:int -> bh:int -> maxit:int -> map:(int -> int -> Complex) -> unit

let mutable fillFun = blockFill
//val mutable fillFun : (int -> int -> int -> (int -> int -> Complex) -> unit)

let clearOffScreen (b : Bitmap) =
    use g = Graphics.FromImage(b)
    g.Clear(Color.White)

let mutable bitmap = new Bitmap(form.Width, form.Height)
let mutable bmpw = form.Width
let mutable bmph = form.Height
//val clearOffScreen : b:Bitmap -> unit
//val mutable bitmap : Bitmap
//val mutable bmpw : int = 800
//val mutable bmph : int = 600

// The following is needed for paint to compile ...
open System.Threading
let mutable rect = Rectangle.Empty
let mutable worker = Thread.CurrentThread
let mutable tl = (-3.0, 2.0)
let mutable br = (2.0, -2.0)

let paint (g : Graphics) =
    lock bitmap (fun () -> g.DrawImage(bitmap, 0, 0))
    g.DrawRectangle(Pens.Black, rect)
    g.FillRectangle(new SolidBrush(Color.FromArgb(128, Color.White)), rect)

let timer = new System.Windows.Forms.Timer(Interval = 100)
timer.Tick.Add(fun _ -> form.Invalidate() )

let stopWorker () =
    if worker <> Thread.CurrentThread then
        worker.Abort()
        worker <- Thread.CurrentThread
//val mutable rect : Rectangle = {X=0,Y=0,Width=0,Height=0}
//val mutable worker : System.Threading.Thread
//val mutable tl : float * float = (-3.0, 2.0)
//val mutable br : float * float = (2.0, -2.0)
//val paint : g:Graphics -> unit
//val timer : Timer = System.Windows.Forms.Timer, Interval: 100
//val stopWorker : unit -> unit

let drawMandel () =
    let bf = fillFun bmpw bmph
    stopWorker()
    timer.Enabled <- true
    worker <- new Thread(fun () -> run bf form bitmap tl br)
    worker.IsBackground <- true
    worker.Priority <- ThreadPriority.Lowest
    worker.Start()
//val drawMandel : unit -> unit

<Mandel iter="1000">
  <topleft>
    <re>-7.47421339220139e-001</re>
    <im>1.64667039391667e-001</im>
  </topleft>
  <bottomright>
    <re>-7.47082959511805e-001</re>
    <im>1.64413254610417e-001</im>
  </bottomright>
</Mandel>

open System.Xml

type CanvasForm() as x =
    inherit Form()
    do x.SetStyle(ControlStyles.OptimizedDoubleBuffer, true)
    override x.OnPaintBackground(args) = ()

// Creates the Form
let form = new CanvasForm(Width = 800, Height = 600,Text = "Mandelbrot set")

let mutable worker = Thread.CurrentThread

let mutable startsel = Point.Empty
let mutable rect = Rectangle.Empty
let mutable tl = (-3.0, 2.0)
let mutable br = (2.0, -2.0)

let mutable menuIterations = 150

let iterations (tlx, tly) (brx, bry) =
    menuIterations

let setCoord (tlx : float, tly : float) (brx : float, bry : float)  =
    let dx = (brx - tlx) / float bmpw
    let dy = (tly - bry) / float bmph
    let mapx x = tlx + float x * dx
    let mapy y = tly - float y * dy
    tl <- (mapx rect.Left, mapy rect.Top)
    br <- (mapx rect.Right, mapy rect.Bottom)

let ensureAspectRatio (tlx : float, tly : float) (brx : float, bry : float) =
    let ratio = (float bmpw / float bmph)
    let w, h = abs(brx - tlx), abs(tly - bry)
    if ratio * h > w then
        br <- (tlx + h * ratio, bry)
    else
        br <- (brx, tly - w / ratio)

let updateView () =
    if rect <> Rectangle.Empty then setCoord tl br
    ensureAspectRatio tl br
    rect <- Rectangle.Empty
    stopWorker()
    clearOffScreen bitmap
    drawMandel()

let click (arg : MouseEventArgs) =
    if rect.Contains(arg.Location) then
        updateView()
    else
        form.Invalidate()
        rect <- Rectangle.Empty
        startsel <- arg.Location

let mouseMove (arg : MouseEventArgs) =
    if arg.Button = MouseButtons.Left then
        let tlx = min startsel.X arg.X
        let tly = min startsel.Y arg.Y
        let brx = max startsel.X arg.X
        let bry = max startsel.Y arg.Y
        rect <- new Rectangle(tlx, tly, brx - tlx, bry - tly)
        form.Invalidate()

let resize () =
    if bmpw <> form.ClientSize.Width ||
       bmph <> form.ClientSize.Height then
         stopWorker()
         rect <- form.ClientRectangle
         bitmap <- new Bitmap(form.ClientSize.Width, form.ClientSize.Height)
         bmpw <- form.ClientSize.Width
         bmph <- form.ClientSize.Height

         updateView()

let zoom amount (tlx, tly) (brx, bry) =
    let w, h = abs(brx - tlx), abs(tly - bry)
    let nw, nh = amount * w, amount * h
    tl <- (tlx + (w - nw) / 2., tly - (h - nh) / 2.)
    br <- (brx - (w - nw) / 2., bry + (h - nh) / 2.)
    rect <- Rectangle.Empty
    updateView()

let selectDropDownItem (l : ToolStripMenuItem) (o : ToolStripMenuItem) =
    for el in l.DropDownItems do
      let item = (el :?> ToolStripMenuItem)
      item.Checked <- (o = item)

let setFillMode (p : ToolStripMenuItem) (m : ToolStripMenuItem) filler _ =
    if (not m.Checked) then
        selectDropDownItem p m
        fillFun <- filler
        drawMandel()

let setupMenu () =
  let m = new MenuStrip()
  let f = new ToolStripMenuItem("&File")
  let c = new ToolStripMenuItem("&Settings")
  let e = new ToolStripMenuItem("&Edit")
  let ext = new ToolStripMenuItem("E&xit")
  let cols = new ToolStripComboBox("ColorScheme")
  let its = new ToolStripComboBox("Iterations")
  let copybmp = new ToolStripMenuItem("Copy &bitmap")
  let copy = new ToolStripMenuItem("&Copy")
  let paste = new ToolStripMenuItem("&Paste")
  let zoomin = new ToolStripMenuItem("Zoom &In")
  let zoomout = new ToolStripMenuItem("Zoom &Out")
  let fillMode = new ToolStripMenuItem("Fill mode")
  let fillModeLinear = new ToolStripMenuItem("Line")
  let fillModeBlock = new ToolStripMenuItem("Block")

  let itchg = fun _ ->
    menuIterations <- System.Int32.Parse(its.Text)
    stopWorker()
    drawMandel()
    c.HideDropDown()
  ext.Click.Add(fun _ -> form.Dispose()) |> ignore

  copybmp.Click.Add(fun _ -> Clipboard.SetDataObject(bitmap))|> ignore
  copybmp.ShortcutKeyDisplayString <- "Ctrl+Shift+C"
  copybmp.ShortcutKeys <- Keys.Control ||| Keys.Shift ||| Keys.C

  copy.Click.Add(fun _ ->
      let maxit = (iterations tl br)
      let tlx, tly = tl
      let brx, bry = br
      Clipboard.SetText(sprintf "<Mandel iter=\"%d\"><topleft><re>%.14e</re><im>%.14e</im></topleft><bottomright><re>%.14e</re><im>%.14e</im></bottomright></Mandel>" maxit tlx tly brx bry)
  ) |> ignore
  copy.ShortcutKeyDisplayString <- "Ctrl+C"
  copy.ShortcutKeys <- Keys.Control ||| Keys.C

  paste.Click.Add(fun _ ->
      if Clipboard.ContainsText() then
        let doc = new XmlDocument()
        try
          doc.LoadXml(Clipboard.GetText())
          menuIterations <- int (doc.SelectSingleNode("/Mandel").Attributes.["iter"].Value)
          tl <- (float (doc.SelectSingleNode("//topleft/re").InnerText), float (doc.SelectSingleNode("//topleft/im").InnerText))
          br <- (float (doc.SelectSingleNode("//bottomright/re").InnerText), float (doc.SelectSingleNode("//bottomright/im").InnerText))
          rect <- Rectangle.Empty
          updateView()
        with _ -> ()
  ) |> ignore
  paste.ShortcutKeyDisplayString <- "Ctrl+V"
  paste.ShortcutKeys <- Keys.Control ||| Keys.V

  zoomin.Click.Add(fun _ -> zoom 0.9 tl br) |> ignore
  zoomin.ShortcutKeyDisplayString <- "Ctrl+T"
  zoomin.ShortcutKeys <- Keys.Control ||| Keys.T
  zoomout.Click.Add(fun _ -> zoom 1.25 tl br) |> ignore
  zoomout.ShortcutKeyDisplayString <- "Ctrl+W"
  zoomout.ShortcutKeys <- Keys.Control ||| Keys.W

  for x in [f; e; c] do m.Items.Add(x) |> ignore
  f.DropDownItems.Add(ext) |> ignore
  let tsi x = (x :> ToolStripItem)
  for x in [tsi cols; tsi its; tsi fillMode] do c.DropDownItems.Add(x) |> ignore
  for x in [tsi copy; tsi paste; tsi copybmp; tsi zoomin; tsi zoomout] do e.DropDownItems.Add(x) |> ignore
  for x in ["HSL Color"; "Gray"; "Red"; "Green"] do cols.Items.Add(x) |> ignore
  fillMode.DropDownItems.Add(fillModeLinear) |> ignore
  fillMode.DropDownItems.Add(fillModeBlock) |> ignore
  cols.SelectedIndex <- 0
  cols.DropDownStyle <- ComboBoxStyle.DropDownList

  cols.SelectedIndexChanged.Add(fun _ ->
    palette <- createPalette coloring.[cols.SelectedIndex]
    stopWorker()
    drawMandel()
    c.HideDropDown()
  )
  its.Text <- string menuIterations
  its.DropDownStyle <- ComboBoxStyle.DropDown
  for x in ["150"; "250"; "500"; "1000"] do its.Items.Add(x) |> ignore
  its.LostFocus.Add(itchg)
  its.SelectedIndexChanged.Add(itchg)
  fillModeBlock.Checked <- true
  fillModeLinear.Click.Add(setFillMode fillMode fillModeLinear linearFill)
  fillModeBlock.Click.Add(setFillMode fillMode fillModeBlock blockFill)
  m

clearOffScreen bitmap
form.MainMenuStrip <- setupMenu()
form.Controls.Add(form.MainMenuStrip)
form.MainMenuStrip.RenderMode <- ToolStripRenderMode.System
form.Paint.Add(fun arg ->  paint arg.Graphics)
form.MouseDown.Add(click)
form.MouseMove.Add(mouseMove)
form.ResizeEnd.Add(fun _ -> resize())
form.Show()

Application.DoEvents()

drawMandel()

[<STAThread>]
do Application.Run(form)

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System.Windows
open System.Windows.Controls

let w = new Window()

let b = new Button(Content = "Hello from WPF!")
b.Click.Add(fun _ -> w.Close())

w.Content <- b
w.Show()

let a = new Application()

[<STAThread>]
do a.Run(w) |> ignore

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"

open System.Windows
open System.Windows.Controls
open System.Windows.Shapes
open System.Windows.Media

let c = new Canvas()
let w = new Window(Topmost = true, Content = c)
w.Show()

let r = new Rectangle(Width = 100., Height = 100.,
                      RadiusX = 5., RadiusY = 5.,
                      Stroke = Brushes.Black)
c.Children.Add(r)

let e = new Ellipse(Width = 150., Height = 150.,
                    Stroke = Brushes.Black)
c.Children.Add(e)
Canvas.SetLeft(e, 100.)
Canvas.SetTop(e, 100.)

e.MouseLeftButtonUp.Add(fun _ ->
    e.Fill <-
        if e.Fill = (Brushes.Yellow :> Brush) then Brushes.Red
        else Brushes.Yellow)
//val c : System.Windows.Controls.Canvas
//val w : System.Windows.Window = System.Windows.Window
//val r : System.Windows.Shapes.Rectangle
//val e : System.Windows.Shapes.Ellipse

(*
// The contents of window.xaml ...
<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="MainWindow" Height="350" Width="525">
    <Canvas>
        <Rectangle Width="100" Height="100" Stroke="Black" 
                   RadiusX="5" RadiusY="10"/>
        <Ellipse Name="Circle"
                 Canvas.Left="100" Canvas.Top="100" 
                 Width="150" Height="150" Stroke="Black"/>
    </Canvas>
</Window>
*)

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Xml

let loadXamlWindow (filename : string) =
    let reader = XmlReader.Create(filename)
    XamlReader.Load(reader) :?> Window

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let wnd = loadXamlWindow("window.xaml")
wnd.Show()
// WARNING: If current directory is not set, the the following exception is thrown ...
// System.IO.FileNotFoundException: Could not find file 'C:\Users\<username>\AppData\Local\Temp\Window.xaml'.

[<STAThread>]
do a.Run(w) |> ignore

// The following references are not needed when running this snippet from F# interactive.
//#r "WindowsBase"
//#r "PresentationCore"
//#r "PresentationFramework"
//#r "System.Xaml"
#r @".\15Visual\packages\FSharpx.TypeProviders.1.6.4\lib\45\Fsharpx.Core.dll"
#r @".\15Visual\packages\FSharpx.TypeProviders.1.6.4\lib\45\Fsharpx.TypeProviders.dll"

open FSharpx

type MainWindow = XAML<"window.xaml">

let mainwnd = new MainWindow()
let wnd = mainwnd.Root
wnd.Show()
//type MainWindow = FSharpx.XAML
//val mainwnd : MainWindow
//val wnd : Window = System.Windows.Window

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Xml
open System.Windows.Shapes

let loadXamlWindow (filename : string) =
    let reader = XmlReader.Create(filename)
    XamlReader.Load(reader) :?> Window

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let w = loadXamlWindow("window.xaml")
w.Show()

let e = w.FindName("Circle") :?> Ellipse

e.MouseLeftButtonUp.Add(fun _ ->
    e.Fill <- 
        if e.Fill = (Brushes.Yellow :> Brush) then Brushes.Red
        else Brushes.Yellow)
//val loadXamlWindow : filename:string -> System.Windows.Window
//val wnd : System.Windows.Window = System.Windows.Window
//val e : System.Windows.Shapes.Ellipse

(*
// The xaml with transformed elements, window2.xaml ...
<Window xmlns="http://schemas.microsoft.com/winfx/2006/xaml/presentation"
        xmlns:x="http://schemas.microsoft.com/winfx/2006/xaml"
        Title="MainWindow" Height="350" Width="525">
    <Canvas>
            <StackPanel Orientation="Horizontal" VerticalAlignment="Center">
                <StackPanel.LayoutTransform>
                    <RotateTransform Angle="-45"/>
                </StackPanel.LayoutTransform>
                <TextBlock>This is a test</TextBlock>
                <TextBox Name="Input">
                    <TextBox.LayoutTransform>
                        <RotateTransform Angle="90"/>
                    </TextBox.LayoutTransform>
                        Input here
                </TextBox>
                <Line X1="0" X2="50" Y1="0" Y2="50" Stroke="Black"/>
            </StackPanel>
    </Canvas>
</Window>
*)
 
let ShowVisualTree(obj : DependencyObject) =
    let rec browse indentlvl node =
        let indent n = for i = 0 to (n - 1) do printf "  "
        let n = VisualTreeHelper.GetChildrenCount(node)
    
        indent indentlvl 
        printfn "<%s%s>" (node.GetType().Name) (if n = 0 then "/" else "")
        for i = 0 to (n - 1) do
            browse (indentlvl + 1) (VisualTreeHelper.GetChild(node, i))
        if n > 0 then
            indent indentlvl
            printfn "</%s>" (node.GetType().Name)

    browse 0 obj
//val ShowVisualTree : obj:DependencyObject -> unit

(*
<Button>
  <ButtonChrome>
    <ContentPresenter>
      <TextBlock/>
    </ContentPresenter>
  </ButtonChrome>
</Button>
*)

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"
#r "System.Xaml"

open System.Windows
open System.Windows.Controls
open System.Windows.Markup
open System.Windows.Media
open System.Xml
open System.Windows.Shapes

let loadXamlWindow (filename : string) =
    let reader = XmlReader.Create(filename)
    XamlReader.Load(reader) :?> Window

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let w = loadXamlWindow("window2.xaml")
w.Show()

let t = w.FindName("Input") :?> TextBox
MessageBox.Show(sprintf "Input value is %s " t.Text) |> ignore
t.Text <- "Hello"
MessageBox.Show(sprintf "Input value now is %s " t.Text) |> ignore
//val loadXamlWindow : filename:string -> System.Windows.Window
//val w : System.Windows.Window = System.Windows.Window
//val t : TextBox = System.Windows.Controls.TextBox: Input here

#load "load_wpf.fsx"
open System.ComponentModel

let desc = 
    DependencyPropertyDescriptor.FromProperty(TextBox.TextProperty, typeof<TextBox>)
desc.AddValueChanged(t, fun _ _ -> MessageBox.Show("Text changed!") |> ignore)
//val desc : System.ComponentModel.DependencyPropertyDescriptor = Text

<TextBlock Text="{Binding Path=Text, ElementName=Input}"></TextBlock>

#r "WindowsBase"
#r "PresentationCore"
#r "PresentationFramework"

open System
open System.IO
open System.Windows.Media.Imaging

let transformFile quality width height srcFileName outFileName =
    let dec = new JpegBitmapDecoder(
                Uri(srcFileName),
                BitmapCreateOptions.PreservePixelFormat,
                BitmapCacheOption.Default)
    let w = dec.Frames.[0].Width
    let h = dec.Frames.[0].Height

    let b = new BitmapImage()
    b.BeginInit()
    b.UriSource <- new Uri(srcFileName)
    if width > 0 then
        if w >= h then b.DecodePixelWidth <- width
        else b.DecodePixelHeight <- height
    b.EndInit()

    let metadata = dec.Frames.[0].Metadata

    let enc = new JpegBitmapEncoder()
    enc.Frames.Add(BitmapFrame.Create(b, null, metadata :?> BitmapMetadata, null))
    let fs = new FileStream(outFileName, FileMode.OpenOrCreate)
    enc.QualityLevel <- quality
    enc.Save(fs)
    fs.Close()
    let fin = new FileInfo(srcFileName)
    let fout = new FileInfo(outFileName)
    fout.CreationTime <- fin.CreationTime
    fout.LastWriteTime <- fin.LastWriteTime

let transformDir quality width height src dest =
    let rec visit (dirIn : DirectoryInfo) (dirOut : DirectoryInfo) =
        for f in dirIn.EnumerateFiles() do
            if f.Extension.ToUpper() = ".JPG" then
                printfn "Processing file %s..." f.FullName
                transformFile 
                    quality width height f.FullName 
                    (dirOut.FullName + "\\" + f.Name)

        for d in dirIn.EnumerateDirectories() do
            visit d (dirOut.CreateSubdirectory(d.Name))

    let dirIn = new DirectoryInfo(src)
    let dirOut = 
        if not(Directory.Exists(dest)) then Directory.CreateDirectory dest 
        else new DirectoryInfo(dest)
    visit dirIn dirOut
//val transformFile :
//  quality:int ->
//    width:int ->
//      height:int -> srcFileName:string -> outFileName:string -> unit
//val transformDir :
//  quality:int -> width:int -> height:int -> src:string -> dest:string -> unit

let dn = @"C:\Users\SomeUser\Pictures\Summer 2010"
let dno = @"e:\Summer PhotoFrame 2010"

transformDir 75 1027 768 dn dno
