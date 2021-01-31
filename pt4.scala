object math{
	import scala.math._

	case class vec3(x:Double, y:Double, z:Double){
		def unary_-():vec3 = vec3(-this.x, -this.y, -this.z)
		def *(that:Double):vec3 = vec3(this.x*that, this.y*that, this.z*that)
		def /(that:Double):vec3 = vec3(this.x/that, this.y/that, this.z/that)

		def +(that: vec3):vec3 = vec3(this.x+that.x, this.y+that.y, this.z+that.z)
		def -(that: vec3):vec3 = vec3(this.x-that.x, this.y-that.y, this.z-that.z)
		def *(that: vec3):vec3 = vec3(this.x*that.x, this.y*that.y, this.z*that.z)
		def /(that: vec3):vec3 = vec3(this.x/that.x, this.y/that.y, this.z/that.z)

		override def toString():String = (this.x.toString)+" "+(this.y.toString)+" "+(this.z.toString)
	}

	def dot(a:vec3, b:vec3) = a.x*b.x + a.y*b.y + a.z*b.z
	def cross(a:vec3, b:vec3):vec3 = vec3(a.y*b.z - a.z*b.y, a.z*b.x - a.x*b.z, a.x*b.y - a.y*b.x)
	def length(a:vec3):Double = sqrt(dot(a,a))
	def normalize(a:vec3):vec3 = {
		val l = length(a)
		a/vec3(l, l, l)
	}


	def tangentspace(n:vec3):List[vec3]={
		val sg = if (n.z<0) -1 else 1
		val a = -1/(sg+n.z)
		val b = n.x * n.y * a
		return List[vec3](
			vec3(1.0 + sg * n.x*n.x * a, sg*b, -sg*n.x),
			vec3(b, sg + n.y*n.y * a, -n.y)
		)
	}

	def sampleCosinedHemi(u1:Double, uu2:Double):vec3={
		val u2 = uu2*Pi*2
		val r = sqrt(u1)
		vec3(r*cos(u2), r*sin(u2), sqrt(1-u1))
	}
}

object data{
	import math._
	import scala.math.sqrt

	case class Ray(o:vec3, d:vec3)

	case class Hitpoint(p:vec3, n:vec3, d:Double, c:vec3, e:vec3)

	case class Camera(p:vec3, f:Double){
		def ray(x:Double, y:Double):Ray = {
			return Ray(p, normalize(vec3(x, this.f, -y)))
		}
	}

	case class Sphere(p:vec3, r:Double, c:vec3, e:vec3){

		def distance(ray:Ray, min:Double, max:Double):Option[Double] = {
			val OP = this.p - ray.o
			val b = dot(ray.d, OP)
			val b2 = b*b
			val c = dot(OP, OP) - r*r

			if(c<b2){
				val t1 = b-sqrt(b2-c)
				if(0<t1) return Some(t1)

				val t2 = b+sqrt(b2-c);
				if(0<t2) return Some(t2)
			}
			return None
		}

		def intersect(ray:Ray, min:Double, max:Double):Option[Hitpoint] = {
			val t = distance(ray,min,max)
			if(t == None || t.get < min || max < t.get) return None
			
			val p = ray.o + ray.d*vec3(t.get,t.get,t.get)
			val n = normalize(p-this.p)
			return Some(Hitpoint(p, n, t.get, this.c, this.e))
		}
	}

}

object pt4{
	import math._
	import data._

	def tonemap(x:Double):Int = {
		if(x>=1) return 255
		else if(x<=0) return 0
		else return (255*x).toInt
	}

	def writePPM(w:Int, h:Int, p:List[vec3], filename:String)={
		import java.io._

		val out=new DataOutputStream(new FileOutputStream(filename))
		out.writeBytes("P6\u000a%d %d\u000a%d\u000a".format(w, h, 255))
		for(v <- p){
			out.writeByte(tonemap(v.x))
			out.writeByte(tonemap(v.y))
			out.writeByte(tonemap(v.z))
		}
	}

	def intersect(ray:Ray, spheres:List[Sphere]):Option[Hitpoint]={
		var tmax:Double = 10000
		var tmin = 0.001
		var hit:Option[Hitpoint]=None

		for(s <- spheres){
			val test = s.intersect(ray, tmin, tmax)
			if (test != None){
				tmax = test.get.d
				hit = test
			}
		}

		if(hit != None && dot(ray.d, hit.get.n)>0)
			hit = Some(Hitpoint(hit.get.p, -hit.get.n, hit.get.d, hit.get.c, hit.get.e))

		return hit
	}

	def pt(spheres:List[Sphere], initialRay:Ray, rng:scala.util.Random):vec3 = {

		var n = 0
		var th = vec3(1,1,1)
		var rad = vec3(0,0,0)
		var ray = initialRay

		while(n<10){
			n = n+1
			val hit = intersect(ray, spheres)
			
			if(hit == None) return rad + th*vec3(0.1,0.1,0.1)
			else{
				val h = hit.get

				rad = rad + th*h.e

				th = th*h.c
				val dtan = sampleCosinedHemi(rng.nextDouble, rng.nextDouble)
				val sptan = tangentspace(h.n)
				ray = Ray(h.p + h.n*0.01, sptan(0)*dtan.x + sptan(1)*dtan.y + h.n*dtan.z)
			}
		}
		
		return rad
	}

	def render(idx:Int, w:Int, h:Int, camera:Camera, spheres:List[Sphere], spp:Int):vec3 = {
		val i = idx%w
		val j = idx/w
		val rng = new scala.util.Random(idx)
		
		var res = vec3(0,0,0)
		for(cnt <- 0 until spp){
			res = res+pt(spheres, camera.ray(i.toDouble/w*2-1, j.toDouble/h*2-1), rng)
		}

		return res/spp
	}

	def main(args: Array[String]) {

		val spheres = List(
			Sphere(vec3( 0.5, 0, 1), 1, vec3(0.8, 0.1, 0.1), vec3(0,0,0)),
			Sphere(vec3(-0.5, 0, 1), 1, vec3(0.1, 0.8, 0.1), vec3(0,0,0)),
			Sphere(vec3(0, 0, -1000), 1000, vec3(0.5, 0.5, 0.5), vec3(0,0,0)),
			Sphere(vec3(0, -1, 4), 0.4, vec3(0,0,0), vec3(30,30,30))
		)

		val camera = Camera(vec3(0, -3.5, 2.5), 1)

		val w = 256
		val h = 256
		val result = List.range(0, w*h).map(i=>render(i, w, h, camera, spheres, 100))
		
		writePPM(w, h, result, "result.ppm")
	}
}