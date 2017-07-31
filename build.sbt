val course = "progfun1"

organization := "sniggel"

name := "coursera-functional-programming-scala"

scalaVersion := "2.11.7"

lazy val week1 = (project in file("week1/recfun"))

lazy val week2 = (project in file("week2/funsets"))

lazy val root = (project in file(".")).
  aggregate(week1, week2)
