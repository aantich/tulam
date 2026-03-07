record Point2D = { x:Int, y:Int };

type Direction = North | South | East | West;

function origin() : Point2D = Point2D(0, 0);
function makePoint(a:Int, b:Int) : Point2D = Point2D(a, b);

function distSquared(p:Point2D) : Int = match
    | Point2D(px, py) -> px * px + py * py;

function manhattan(p:Point2D, q:Point2D) : Int = match
    | Point2D(px, py), Point2D(qx, qy) -> abs(px - qx) + abs(py - qy);

function translate(p:Point2D, dx:Int, dy:Int) : Point2D = match
    | Point2D(px, py), dx, dy -> Point2D(px + dx, py + dy);

function moveDir(p:Point2D, d:Direction) : Point2D = match
    | p, North -> translate(p, 0, 1)
    | p, South -> translate(p, 0, 0 - 1)
    | p, East  -> translate(p, 1, 0)
    | p, West  -> translate(p, 0 - 1, 0);

function midpoint(p:Point2D, q:Point2D) : Point2D = match
    | Point2D(px, py), Point2D(qx, qy) ->
        Point2D(div(px + qx, 2), div(py + qy, 2));
