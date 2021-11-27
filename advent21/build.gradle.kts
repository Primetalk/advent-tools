plugins {
    scala
//    id("com.github.maiflai.scalatest") version "0.26"
}
//
repositories {
    mavenCentral()// jcenter()
}
//
val scalaMinorVersion = "3.1"
val scalaVersion      = "3.1.0"
//val scalaTestVersion  = "3.2.3"
//
dependencies {
//    implementation("org.scala-lang:scala3-library_3:3.1.0")
    implementation("org.scala-lang:scala3-library_3:$scalaVersion")
//    implementation("com.lihaoyi:fastparse_$scalaMinorVersion:2.3.0")
//    implementation("ru.primetalk:rewritable-tree_$scalaMinorVersion:0.1.0")
////    implementation("org.typelevel:spire_$scalaMinorVersion:0.16.2")
//
//    testImplementation("org.scalatest:scalatest-flatspec_$scalaMinorVersion:$scalaTestVersion")
//    testImplementation("org.scalatest:scalatest-shouldmatchers_$scalaMinorVersion:$scalaTestVersion")
//    testImplementation("org.scalatest:scalatest-propspec_$scalaMinorVersion:$scalaTestVersion")
//    testImplementation("org.scalatest:scalatest-core_$scalaMinorVersion:$scalaTestVersion")
//
//    testImplementation("org.scalatest:scalatest_$scalaMinorVersion:$scalaTestVersion")
//    testImplementation("org.scalatestplus:scalacheck-1-14_$scalaMinorVersion:3.2.2.0")
//
////    testImplementation("org.scalacheck:scalacheck_$scalaMinorVersion:1.13.5")
//    testImplementation("com.chuusai:shapeless_$scalaMinorVersion:2.3.3")
//
//    testRuntimeOnly("com.vladsch.flexmark:flexmark-all:0.36.8")
}
//
//tasks.test {
//    maxParallelForks = 2
//}
//
tasks.register<JavaExec>("Day2100"){
    group = "run"
    description = "Run Day2100"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2021.Day2100")
}
