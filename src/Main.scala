import org.apache.poi.xssf.usermodel.XSSFWorkbook
import utils.SVar

import java.io.FileInputStream
import scala.util.matching.Regex
object Main {
  def main(args: Array[String]): Unit = {

    def sv_if(condition: String)(run: => Unit) ={}
      val trans_pat: Regex = ".+#\\d.*%(?:\\((.+)\\))?".r
      val trans_pat(tmp) = "data[#1]%(name)"
//      println(tmp)
    val tt = SVar.extPatTrans(new SVar("", List(1), "ext", "", "{data[#0][7:0], p[#1]}%(0:5, 5)"))
    println(tt)
    val file_path = raw"C:\Users\liyue\Desktop\sv_gen.xlsx"
    val xss = new FileInputStream(file_path)
    val excel =new XSSFWorkbook(xss)

//    val sheet = xss.getSheetAt(1)
//    val t2 = SVar(sheet.getRow(2))
//    println(t2)
  }
}