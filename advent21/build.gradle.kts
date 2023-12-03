plugins {
    scala
    id("com.github.maiflai.scalatest") version "0.32"
}

tasks.withType<ScalaCompile>{
    scalaCompileOptions.additionalParameters = listOf("-feature")
}

repositories {
    mavenCentral()
}

val scalaMinorVersion = "3.3"
val scalaVersion      = "3.3.1"

dependencies {
    implementation("org.scala-lang:scala3-library_3:$scalaVersion")
    implementation("org.typelevel:cats-parse_3:1.0.0")
    implementation("org.typelevel:cats-collections-core_3:0.9.8")
    implementation("org.typelevel:cats-effect_3:3.5.2")
    implementation("org.scala-lang.modules:scala-parallel-collections_3:1.0.4")
    
    implementation("eu.timepit:refined_3:0.11.0")
    
    testImplementation("org.scalatest:scalatest_3:3.2.17")
    testRuntimeOnly("com.vladsch.flexmark:flexmark-all:0.64.8")
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

tasks.register<JavaExec>("Day2301"){
    group = "run"
    description = "Run Day2301"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2023.Day2301")
}

tasks.register<JavaExec>("Day2303"){
    group = "run"
    description = "Run Day2301"
    classpath = sourceSets.main.get().runtimeClasspath
    mainClass.set("org.primetalk.advent2023.Day2303")
}
