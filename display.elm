arcs: [Arc]
arcs = [{a=0, b=100, n=9},
        {a=10, b=100, n=9},
        {a=13, b=160, n=40},
        {a=43, b=53, n=2},
        {a=15, b=40, n=4},
        {a=70, b=100, n=2},
        {a=100, b=120, n=9}g
        ]

x = 1000

config: Config
config = {w=x,h=x}

main: Element
main = display config (scaleArcs arcs (toFloat config.w))

--

type Arc = {a: Float, b: Float, n: Float}
type Config = {w: Int, h: Int}

display: Config -> [Arc] -> Element
display c arcs = 
  let arcforms = map (arc2form c) arcs
  in collage c.w c.h arcforms
 
arc2form: Config -> Arc-> Form
arc2form q arc = 
  let line = {width=arc.n, color=(rgba 78 154 255 0.5), cap=Flat, join=Smooth, dashing=[], dashOffset=0}
      r = (arc.b - arc.a) / 2
      cx = arc.a + (arc.n/2) + r
      cy = 0
      offset = (-(toFloat q.w)/2, -(toFloat q.h)/2)
  in traced line (path (arcPoints (cx, cy) r)) |> move offset

arcPoints: (number,number) -> number -> [(Float, Float)]
arcPoints center r = 
  let theta = linspace pi 0 1000
      x t = (fst center) + r * cos t
      y t = (snd center) + r * sin t
      f t = (x t, y t)
  in map f theta
        
linspace: number -> number -> Int -> [number]
linspace start end n = 
  let dt = (end - start) / ((toFloat n) - 1)
      f i = start + (i * dt)
  in map f [0..(toFloat n)-1]
  
  
scaleArcs: [Arc] -> Float -> [Arc]
scaleArcs arcs width = 
  let s = width / (maximum (map (\a -> a.b + a.n) arcs)) -- scalefactor
  in map (\x -> {a=x.a*s, b=x.b*s, n=x.n*s}) arcs


















