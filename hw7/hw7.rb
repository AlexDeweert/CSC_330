# Homework 7, hw7.rb (see also SML code)
# CSC 330 UVIC Fall 2018
# Prof. D.M. German
# Student: Alex (Lee) DEWEERT
# ID: V00855767

class GeometryExpression
  # do *not* change this class definition
  Epsilon = 0.00001
end

class GeometryValue
  # do *not* change methods in this class definition
  # you can add methods if you wish

  private
  # some helper methods that may be generally useful
  def real_close(r1,r2)
      (r1 - r2).abs < GeometryExpression::Epsilon
  end
  def real_close_point(x1,y1,x2,y2)
      real_close(x1,x2) && real_close(y1,y2)
  end
  # two_points_to_line could return a Line or a VerticalLine
  def two_points_to_line(x1,y1,x2,y2)
      if real_close(x1,x2)
        VerticalLine.new x1
      else
        m = (y2 - y1).to_f / (x2 - x1)
        b = y1 - m * x1
        Line.new(m,b)
      end
  end

  public
  # we put this in this class so all subclasses can inherit it:
  # the intersection of self with a NoPoints is a NoPoints object
  def intersectNoPoints np
    np # could also have NoPoints.new here instead
  end

  # we put this in this class so all subclasses can inhert it:
  # the intersection of self with a LineSegment is computed by
  # first intersecting with the line containing the segment and then
  # calling the result's intersectWithSegmentAsLineResult with the segment
  def intersectLineSegment seg
    line_result = intersect(two_points_to_line(seg.x1,seg.y1,seg.x2,seg.y2))
    line_result.intersectWithSegmentAsLineResult seg
  end
end

class NoPoints < GeometryValue
  # do *not* change this class definition: everything is done for you
  # (although this is the easiest class, it shows what methods every subclass
  # of geometry values needs)

  # Note: no initialize method only because there is nothing it needs to do
  def eval_prog env
    self # all values evaluate to self
  end
  def preprocess_prog
    self # no pre-processing to do here
  end
  def shift(dx,dy)
    self # shifting no-points is no-points
  end
  def intersect other
    other.intersectNoPoints self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint p
    self # intersection with point and no-points is no-points
  end
  def intersectLine line
    self # intersection with line and no-points is no-points
  end
  def intersectVerticalLine vline
    self # intersection with line and no-points is no-points
  end
  # if self is the intersection of (1) some shape s and (2)
  # the line containing seg, then we return the intersection of the
  # shape s and the seg.  seg is an instance of LineSegment
  def intersectWithSegmentAsLineResult seg
    self
  end
end


class Point < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods

  # Note: You may want a private helper method like the local
  # helper function inbetween in the ML code
  attr_reader :x, :y
  def initialize(x,y)
    @x = x
    @y = y
  end
  def eval_prog env
    self
  end
  def preprocess_prog
    self
  end
  def shift(dx,dy)
    Point.new(@x+dx,@y+dy)
  end
  def intersect other
    other.intersectPoint self # will be NoPoints but follow double-dispatch
  end
  def intersectPoint v
    if real_close_point(v.x,v.y,@x,@y)
      Point.new(@x,@y)
    else
      NoPoints.new()
    end
  end
  def intersectLine v
    if real_close(@y, v.m*@x+v.b)
      Point.new(@x,@y)
    else
      NoPoints.new()
    end
  end
  def intersectVerticalLine v
    if real_close(@x, v.x)
      Point.new(@x,@y)
    else
      NoPoints.new()
    end
  end
  def intersectWithSegmentAsLineResult seg
    #x0 = @x, y0 = @y
    if inbetween(@x,seg.x1,seg.x2) && inbetween(@y,seg.y1,seg.y2)
      Point.new(@x,@y)
    else
      NoPoints.new()
    end
  end
  def inbetween (v,end1,end2)
    (end1-GeometryExpression::Epsilon <= v && v <= end2 + GeometryExpression::Epsilon) ||
    (end2-GeometryExpression::Epsilon <= v && v <= end1 + GeometryExpression::Epsilon)
  end

end

class Line < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :m, :b
  def initialize(m,b)
    @m = m
    @b = b
  end
  def eval_prog env
    self
  end
  def preprocess_prog
    self
  end
  def shift(dx,dy)
    Line.new(@m,@b+dy-@m*dx)
  end
  def intersect other
    other.intersectLine self
  end
  def intersectPoint v
    v.intersectLine(self)
  end
  def intersectLine v
    if real_close(@m,v.m) then
      if real_close(@b,v.b)
        Line.new(@m,@b)
      else
        NoPoints.new()
      end
    else
      x = (v.b-@b)/(@m-v.m)
      y = @m*x+@b
      Point.new(x,y)
    end
  end
  def intersectVerticalLine v
    Point.new(v.x,@m*v.x+@b)
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class VerticalLine < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  attr_reader :x
  def initialize x
    @x = x
  end
  def eval_prog env
    self
  end
  def preprocess_prog
    self
  end
  def shift(dx,dy)
    VerticalLine.new(@x+dx)
  end
  def intersect other
    other.intersectVerticalLine self
  end
  def intersectPoint v
    v.intersectVerticalLine(self)
  end
  def intersectLine v
    v.intersectVerticalLine(self)
  end
  def intersectVerticalLine v
    if real_close(@x,v.x)
      self
    else
      NoPoints.new()
    end
  end
  def intersectWithSegmentAsLineResult seg
    seg
  end
end

class LineSegment < GeometryValue
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: This is the most difficult class.  In the sample solution,
  #  preprocess_prog is about 15 lines long and
  # intersectWithSegmentAsLineResult is about 40 lines long
  attr_reader :x1, :y1, :x2, :y2
  def initialize (x1,y1,x2,y2)
    @x1 = x1
    @y1 = y1
    @x2 = x2
    @y2 = y2
  end
  def eval_prog env
    self
  end
  def preprocess_prog
    if real_close(@x1,@x2) && real_close(@y1,@y2)
      Point.new(@x1,@y1)
    elsif !real_close(@x1,@x2) && @x1 > @x2 || real_close(@x1,@x2) && @y1 > @y2
      LineSegment.new(@x2,@y2,@x1,@y1)
    elsif @x1 < @x2 || real_close(@x1,@x2) && @y1 < @y2
      self
    end
  end
  def shift(dx,dy)
    LineSegment.new(@x1+dx,@y1+dy,@x2+dx,@y2+dy)
  end
  def intersect other
    other.intersectLineSegment self # will be NoPoints but follow double-dispatch
  end
  def intersectWithSegmentAsLineResult seg
    x1start = @x1
    y1start = @y1
    x1end = @x2
    y1end = @y2
    x2start = seg.x1
    y2start = seg.y1
    x2end = seg.x2
    y2end = seg.y2

    #default if y1start < y2start
    aXstart = @x1
    aYstart = @y1
    aXend = @x2
    aYend = @y2
    bXstart = seg.x1
    bYstart = seg.y1
    bXend = seg.x2
    bYend = seg.y2

    if real_close(x1start,x1end)
        if !(y1start < y2start)
          bXstart = @x1
          bYstart = @y1
          bXend = @x2
          bYend = @y2
          aXstart = seg.x1
          aYstart = seg.y1
          aXend = seg.x2
          aYend = seg.y2
        end
        if real_close(aYend,bYstart)
          Point.new(aXend,aYend)
        elsif aYend < bYstart
          NoPoints.new()
        elsif aYend > bYend
          LineSegment.new(bXstart,bYstart,bXend,bYend)
        else
          LineSegment.new(bXstart,bYstart,aXend,aYend)
        end
    else
      if !(x1start < x2start)
          bXstart = @x1
          bYstart = @y1
          bXend = @x2
          bYend = @y2
          aXstart = seg.x1
          aYstart = seg.y1
          aXend = seg.x2
          aYend = seg.y2
      end
      if real_close(aXend,bXstart)
        Point.new(aXend,aYend)
      elsif aXend < bXstart
        NoPoints.new()
      elsif aXend > bXend
        LineSegment.new(bXstart,bYstart,bXend,bYend)
      else
        LineSegment.new(bXstart,bYstart,aXend,aYend)
      end
    end
  end
  def intersectPoint v
    v.intersectLineSegment self
  end
  def intersectLine v
    v.intersectLineSegment self
  end
  def intersectVerticalLine v
    v.intersectLineSegment self
  end
end

# Note: there is no need for getter methods for the non-value classes
class Intersect < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(e1,e2)
    @e1 = e1
    @e2 = e2
  end
  def eval_prog env
    @e1.eval_prog(env).intersect @e2.eval_prog(env)
  end
  def preprocess_prog
    Intersect.new(@e1.preprocess_prog,@e2.preprocess_prog)
  end
end

class Let < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  # Note: Look at Var to guide how you implement Let
  def initialize(s,e1,e2)
    @s = s
    @e1 = e1
    @e2 = e2
  end
  def eval_prog env
    #creates  a copy of env
    new_env = Array.new(env)
    #adds the Let binding to the new env
    #but adds at the beginning for shadowing to
    #work (since Var does left-right lookup)
    new_env.unshift([@s,@e1.eval_prog(env)])
    #returns the evaluation of e2 with the newly
    #created environment
    @e2.eval_prog(new_env)
  end
  def preprocess_prog
    Let.new(@s,@e1.preprocess_prog,@e2.preprocess_prog)
  end
end

class Var < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize s
    @s = s
  end
  def eval_prog env # remember: do not change this method
    pr = env.assoc @s
    raise "undefined variable" if pr.nil?
    #puts "Var eval_prog => got: " + pr[0].to_s + " " + pr[1].to_s
    pr[1]
  end
  def preprocess_prog
    self
  end
end

class Shift < GeometryExpression
  # *add* methods to this class -- do *not* change given code and do not
  # override any methods
  def initialize(dx,dy,e)
    @dx = dx
    @dy = dy
    @e = e
  end
  def eval_prog env
    @e.eval_prog(env).shift(@dx,@dy)
  end
  def preprocess_prog
    Shift.new(@dx,@dy,@e.preprocess_prog)
  end
end
