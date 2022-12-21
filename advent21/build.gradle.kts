plugins {
    scala
    id("com.github.maiflai.scalatest") version "0.31"
}

repositories {
    mavenCentral()
}

val scalaMinorVersion = "3.2"
val scalaVersion      = "3.2.1"

dependencies {
    implementation("org.scala-lang:scala3-library_3:$scalaVersion")
    implementation("org.typelevel:cats-parse_3:0.3.6")
    implementation("org.typelevel:cats-collections-core_3:0.9.3")
    implementation("org.typelevel:cats-effect_3:3.4.2")
    implementation("org.scala-lang.modules:scala-parallel-collections_3:1.0.4")
    
    testImplementation("org.scalatest:scalatest_3:3.2.10")
    testRuntimeOnly("com.vladsch.flexmark:flexmark-all:0.62.2")

}

tasks.test {
   maxParallelForks = 2
}

tasks.register<JavaExec>("Day2100"){
    group = "run"
    description = "Run Day2100"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2021.Day2100")
}

tasks.register<JavaExec>("Day2110"){
    group = "run"
    description = "Run Day2110"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2021.Day2110")
}

tasks.register<JavaExec>("Day2115"){
    group = "run"
    description = "Run Day2115"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2021.Day2115")
}

tasks.register<JavaExec>("Day2120"){
    group = "run"
    description = "Run Day2120"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2021.Day2120")
}

tasks.register<JavaExec>("Day2121"){
    group = "run"
    description = "Run Day2121"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2021.Day2121")
}
