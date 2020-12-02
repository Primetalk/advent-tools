plugins {
    scala
    id("com.github.maiflai.scalatest") version "0.26"
}

repositories {
    mavenCentral()// jcenter()
}

val scalaMinorVersion = "2.13"
val scalaVersion      = "2.13.4"
val scalaTestVersion  = "3.2.3"

dependencies {
    implementation("org.scala-lang:scala-library:$scalaVersion")
    implementation("com.lihaoyi:fastparse_$scalaMinorVersion:2.3.0")
    implementation("ru.primetalk:rewritable-tree_$scalaMinorVersion:0.1.0")
//    implementation("org.typelevel:spire_$scalaMinorVersion:0.16.2")

    testImplementation("org.scalatest:scalatest-flatspec_$scalaMinorVersion:$scalaTestVersion")
    testImplementation("org.scalatest:scalatest-shouldmatchers_$scalaMinorVersion:$scalaTestVersion")
    testImplementation("org.scalatest:scalatest-propspec_$scalaMinorVersion:$scalaTestVersion")
    testImplementation("org.scalatest:scalatest-core_$scalaMinorVersion:$scalaTestVersion")

    testImplementation("org.scalatest:scalatest_$scalaMinorVersion:$scalaTestVersion")
    testImplementation("org.scalatestplus:scalacheck-1-14_$scalaMinorVersion:3.2.2.0")

//    testImplementation("org.scalacheck:scalacheck_$scalaMinorVersion:1.13.5")
    testImplementation("com.chuusai:shapeless_$scalaMinorVersion:2.3.3")

    testRuntimeOnly("com.vladsch.flexmark:flexmark-all:0.36.8")
}

tasks.test {
    maxParallelForks = 2
}

tasks.register<JavaExec>("Day2002"){
    group = "run"
    description = "Run Day2002"
    classpath = sourceSets.main.get().runtimeClasspath
    main = "org.primetalk.advent2020.Day2002"
}
