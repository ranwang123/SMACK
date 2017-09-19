import scala.xml.Elem
import scala.xml.XML

/**
  * From console.
  * scala MavenToSBTDependencies _pom_file_ {sbt or scala}
 */
object MavenToSBTDependencies{
  
  case class Dep(scala_? : Boolean, vals : List[String]){
  
    def toDep() = {
      if(scala_?){ 
        vals map(v => "\"" + v + "\"") match{
          case head :: tails => head + " %% " + tails.mkString(" % ")
          case Nil => ""
        }
      }
      else{
        vals.map(v => "\"" + v + "\"").mkString(" % ")
      }
    }
  
  }
  
    val ScalaArtifactPattern = """(_\d\.\d\.\d(-\d)?)|(_\$\{scala\.version\})$""".r
  
  def convert( pom : Elem) = {
    
    val deps = pom \ "dependencies" \ "dependency" map( dep => {
      val groupId = dep \ "groupId" text
      val _artifactId = dep \ "artifactId" text
      val version = dep \ "version" text
      val _scope = dep \ "scope" text
      
      val scala_? = !ScalaArtifactPattern.findFirstIn(_artifactId).isEmpty
      val artifactId = if(scala_?){
        _artifactId.substring(0,_artifactId.lastIndexOf("_"))
      }else _artifactId
      
      val scope = _scope match{
        case "compile" => ""
        case "system" => "provided"
        case s => s
      }
      
      if(scope.length > 0){
        Dep(scala_?,groupId :: artifactId :: version :: scope :: Nil)
      }else{
        Dep(scala_?,groupId :: artifactId :: version :: Nil)
      }
    })
    deps.filterNot( _ match{
      case Dep(false, List("org.scala-lang", "scala-library" ,_)) => true
      case _ => false
    })
  }
  
  def toBuildSbt(deps : Seq[Dep]) = {
    """libraryDependencies ++= Seq(
  %s
)""".format(deps.map(_.toDep).mkString(",\r\n  "))
  }
  
  def toBuildScala(deps : Seq[Dep]) = {
    val scopeGroups = deps.groupBy( d => {
      if(d.vals.length == 4) d.vals(3)
      else "compile"
    })
    
    val builder = new StringBuilder
    var libs : List[String] = Nil
    if(scopeGroups.contains("compile")){
      builder.append("""  val libraries = Seq(
    %s
  )
""".format(scopeGroups("compile").map(_.toDep).mkString(",\r\n    ")))
      libs = libs :+ "libraries"
    }
    
    if(scopeGroups.contains("test")){
      builder.append("""  val testLibraries = Seq(
    %s
  )
""".format(scopeGroups("test").map(_.toDep).mkString(",\r\n    ")))
      libs = libs :+ "testLibraries"
    }
      
    if(scopeGroups.contains("provided")){
      builder.append("""  val providedLibraries = Seq(
    %s
  )
""".format(scopeGroups("provided").map(_.toDep).mkString(",\r\n    ")))
      libs = libs :+ "providedLibraries"
    }
      builder.toString + "\r\n" + """libraryDependencies ++= %s""".format(libs.mkString(" ++ "))
  
  }
  
  def m2s( fileName : String) = {
    val pom = XML.loadFile(fileName)
    toBuildScala(convert(pom))
  }
  
  
  def m2s_2( fileName : String) = {
    val pom = XML.loadFile(fileName)
    toBuildSbt(convert(pom))
  }
  
  def main(args : Array[String]) {
    val fileName = args(0)
    val mode = if(args.length > 1) args(1) else "scala"
    mode match{
      case "scala" => println(m2s(fileName))
      case _ => println(m2s_2(fileName))
    }
  }
  
}

/*
// these are test
val regex = MavenToSBTDependencies.ScalaArtifactPattern

regex.findFirstIn("feiao_2.9.1")
regex.findFirstIn("feiao_2.9.0-1")
regex.findFirstIn("feiao_2.9.0")

  regex.findFirstIn("feiao_${scala.version}")

val elem = <hoge>
  <dependencies>
  <dependency>
    <groupId>fuga</groupId>
    <artifactId>aaa_2.9.1</artifactId>
    <version>2323</version>
    <scope>test</scope>
  </dependency>
  <dependency>
    <groupId>fuga</groupId>
<artifactId>{"aaa_${scala.version}"}</artifactId>
    <version>2323</version>
    <scope>compile</scope>
  </dependency>
  <dependency>
    <groupId>fuga</groupId>
<artifactId>{"aaa_${scala.version}"}</artifactId>
    <version>2323</version>
    <scope>provided</scope>
  </dependency>
  <dependency>
    <groupId>fuga</groupId>
    <artifactId>aaa</artifactId>
    <version>2323</version>
  </dependency>
  </dependencies>
</hoge>

MavenToSBTDependencies.toBuildSbt(MavenToSBTDependencies.convert(elem))
MavenToSBTDependencies.toBuildScala(MavenToSBTDependencies.convert(elem))
*/