package pplAssignment
object F2016A7PS0073P {
  def dotProduct(kernel1:List[List[Double]],kernel2:List[List[Double]]): Double ={

    def listProd(l1: List[Double], l2: List[Double], prod:Double): Double ={
      if(l1.isEmpty || l2.isEmpty)   prod;
      else listProd(l1.tail,l2.tail, prod+l1.head*l2.head);
    }

    def dotUtil(m1:List[List[Double]],m2:List[List[Double]], x:Double):Double={
      if(m1.isEmpty || m2.isEmpty){
        x;
      }
      else{
        dotUtil(m1.tail,m2.tail,x+listProd(m1.head,m2.head,0.0));
      }
    }
    dotUtil(kernel1,kernel2,0.0);
  }
  def getMatrix(image:List[List[Double]], rstart:Int,colstart:Int,rend:Int,colend:Int):List[List[Double]]={
    def getRows(image:List[List[Double]], currImage:List[List[Double]],rstart:Int,rend:Int,currRow:Int):List[List[Double]]={
      if(currRow<rstart)  getRows(image.tail,currImage,rstart,rend,currRow+1)
      else if(currRow>=rstart && currRow<=rend && !image.isEmpty){
        getRows(image.tail,currImage:::(image.head::Nil),rstart,rend,currRow+1)
      }
      else currImage
    }

    def getColUtil(list: List[Double], currList: List[Double], index: Int, colstart: Int, colend: Int): List[Double] = {
      if (index < colstart) getColUtil(list.tail, currList, index + 1, colstart, colend)
      else if (index >= colstart && index <= colend && !list.isEmpty) {
        getColUtil(list.tail, currList ::: (list.head :: Nil), index + 1, colstart, colend)
      }
      else currList
    }
    def getCols(image:List[List[Double]], currImage:List[List[Double]],colstart:Int,colend:Int):List[List[Double]]={
      if(image.isEmpty) currImage
      else{
        getCols(image.tail,currImage:::(getColUtil(image.head,Nil,0,colstart,colend)::Nil),colstart,colend)
      }
    }
    getCols(getRows(image,Nil,rstart,rend,0),Nil,colstart,colend)
  }
  def convolute(image:List[List[Double]],kernel1:List[List[Double]],imagesize:List[Int],kernelsize:List[Int]):List[List[Double]]={
    val row=imagesize.head-kernelsize.head
    val col=imagesize.tail.head-kernelsize.tail.head
    def convUtil(image:List[List[Double]],kernel1:List[List[Double]],finalList:List[Double],currRow:Int,currCol:Int,finalRow:Int,finalCol:Int):List[Double]={
      if(currCol<=finalCol){
        convUtil(image,kernel1,finalList:::(dotProduct(getMatrix(image,currRow,currCol,currRow+kernelsize.head,currCol+kernelsize.tail.head),kernel1)::Nil),currRow,currCol+1,finalRow,finalCol)
      }
      else{
        finalList
      }
    }
    def conv(image:List[List[Double]],kernel1:List[List[Double]],finalImage:List[List[Double]],currRow:Int,finalRow:Int):List[List[Double]]={
      if(currRow<=finalRow){
        conv(image,kernel1,finalImage:::(convUtil(image,kernel1,Nil,currRow,0,currRow+kernelsize.head,col)::Nil),currRow+1,finalRow)
      }
      else finalImage
    }
    conv(image,kernel1,Nil,0,row)
  }
  def activationLayer(f:Double=>Double,image:List[List[Double]]):List[List[Double]]={
    def modifyRow(f:Double=>Double,row:List[Double], finalrow:List[Double]):List[Double]={
      if(row.isEmpty) finalrow
      else modifyRow(f,row.tail,finalrow:::(f(row.head)::Nil))
    }

    def activationUtil(f:Double=>Double,image:List[List[Double]], res:List[List[Double]]):List[List[Double]]={
      if(image.isEmpty){
        res
      }
      else    activationUtil(f,image.tail, res:::(modifyRow(f,image.head, Nil)::Nil))
    }
    activationUtil(f,image,Nil)
  }
  def singlePooling(f:List[Double]=>Double,poolmat:List[List[Double]],size:Int):List[Double]={

    def createList(matrix:List[List[Double]]):List[Double]={
      def listUtil(matrix:List[List[Double]], list: List[Double]):List[Double]={
        if(matrix.isEmpty)  list
        else{
          listUtil(matrix.tail, list:::matrix.head)
        }
      }
      listUtil(matrix,Nil)
    }
    def maxCol(poolmat:List[List[Double]]): Int={
      val row = poolmat.head
      def maxColUtil(list:List[Double],col:Int ):Int={
        if(list.isEmpty)  col
        else maxColUtil(list.tail, col+1)
      }
      maxColUtil(row,0)
    }
    val maxcol=maxCol(poolmat)
    def poolUtil(f:List[Double]=>Double,poolmat:List[List[Double]],size:Int, currCol:Int, finalList:List[Double]):List[Double]={
      if(currCol<maxcol){
        val temp:List[Double] = List(f(createList(getMatrix(poolmat,0,currCol,size-1,currCol+size-1))))
        poolUtil(f,poolmat,size,currCol+size,finalList:::temp)
      }
      else finalList
    }
    poolUtil(f,poolmat,size,0,Nil)
  }
  def poolingLayer(f:List[Double]=>Double,image:List[List[Double]],size:Int):List[List[Double]]={
    def traverse(image:List[List[Double]], size:Int):List[List[Double]]={
      if(size==0 || image.isEmpty) image
      else traverse(image.tail,size-1)
    }
    def poolLayerUtil(image:List[List[Double]],finalImage:List[List[Double]],size:Int):List[List[Double]]={
      val layer = traverse(image,size)
      //println(layer)
      if(image.isEmpty) finalImage
      else{
        poolLayerUtil(layer,finalImage:::(singlePooling(f,image,size)::Nil),size)
      }
    }
    poolLayerUtil(image,Nil,size)
  }
  def normalise(nmat:List[List[Double]]):List[List[Int]]={
    def modifyRow(f:Double=>Int,row:List[Double], finalrow:List[Int]):List[Int]={
      if(row.isEmpty) finalrow
      else{
        val temp:List[Int]=List(f(row.head))
        modifyRow(f,row.tail,finalrow:::temp)
      }
    }

    def normaliseUtil(f:Double=>Int,image:List[List[Double]], res:List[List[Int]]):List[List[Int]]={
      if(image.isEmpty){
        res
      }
      else    normaliseUtil(f,image.tail, res:::(modifyRow(f,image.head, Nil)::Nil))
    }
    def Max(image:List[List[Double]]):Double={
      def maxUtil(row:List[Double],max:Double):Double={
        if(row.isEmpty) max
        else{
          if(row.head>max)  maxUtil(row.tail,row.head)
          else maxUtil(row.tail,max)
        }
      }
      def findMax(image:List[List[Double]],max:Double):Double={
        if(image.isEmpty) max
        else{
          findMax(image.tail,maxUtil(image.head,max))
        }
      }
      findMax(image,image.head.head)
    }

    def Min(image: List[List[Double]]): Double = {
      def minUtil(row: List[Double], min: Double): Double = {
        if (row.isEmpty) min
        else {
          if (row.head < min) minUtil(row.tail, row.head)
          else minUtil(row.tail, min)
        }
      }

      def findMin(image: List[List[Double]], min: Double): Double = {
        if (image.isEmpty) min
        else {
          findMin(image.tail, minUtil(image.head, min))
        }
      }

      findMin(image, image.head.head)
    }
    val maximum:Double=Max(nmat)
    val minimum:Double=Min(nmat)
    normaliseUtil((x:Double)=>((x-minimum)*255/(maximum-minimum)).round.toInt,nmat,Nil)
  }
  def mixedLayer(image:List[List[Double]],kernel1:List[List[Double]],imagesize:List[Int],kernel1size:List[Int],actfunc:Double=>Double,poolfunc:List[Double]=>Double,size:Int):List[List[Double]]={
    val res1=convolute(image,kernel1,imagesize,kernel1size)

    val res2=activationLayer(actfunc,res1)

    val res3=poolingLayer(poolfunc,res2,size)

    res3
  }
  def assembly(image:List[List[Double]],imagesize:List[Int],w1:Double,w2:Double,b:Double,kernel1:List[List[Double]],kernel1size:List[Int],kernel2:List[List[Double]],kernel2size:List[Int],kernel3:List[List[Double]],kernel3size:List[Int],size:Int):List[List[Int]]={

    def add(mat1:List[List[Double]],mat2:List[List[Double]], w1:Double,w2:Double, b:Double):List[List[Double]]={
      def addRow(list1:List[Double],list2:List[Double],w1:Double,w2:Double, b:Double,finalList:List[Double]):List[Double]={
        if(list1.isEmpty || list2.isEmpty)  finalList
        else{
          addRow(list1.tail,list2.tail,w1,w2,b,finalList:::((list1.head*w1+list2.head*w2+b)::Nil))
        }
      }
      def addUtil(mat1:List[List[Double]],mat2:List[List[Double]], w1:Double,w2:Double, b:Double, res:List[List[Double]]):List[List[Double]]={
        if(mat1.isEmpty || mat2.isEmpty)  return res
        else{
          addUtil(mat1.tail,mat2.tail,w1,w2,b,res:::(addRow(mat1.head,mat2.head,w1,w2,b,Nil)::Nil))
        }
      }
      addUtil(mat1,mat2,w1,w2,b,Nil)
    }
    def max(xs: List[Double]): Double = {
      if(xs.tail.nonEmpty){
        val tl = max(xs.tail)
        if(tl > xs.head) tl
        else xs.head
      }else{
        xs.head
      }
    }
    def avg(list:List[Double]):Double={
      def avgUtil(list:List[Double], sum:Double , count:Int):Double={
        if(list.isEmpty && count!=0){
          val ret:Double=sum/count
          ret
        }
        else{
          avgUtil(list.tail,sum+list.head,count+1)
        }
      }
      avgUtil(list,0,0)
    }
    def getRow(image:List[List[Double]]):Int={
      def Util(image:List[List[Double]], count:Int):Int={
        if(image.isEmpty) count
        else Util(image.tail,count+1)
      }
      Util(image,0)
    }
    def getCol(image:List[List[Double]]):Int={
      if(image.isEmpty) 0
      else{
        val row=image.head
        def Util(row:List[Double],count:Int):Int={
          if(row.isEmpty) count
          else{
            Util(row.tail,count+1)
          }
        }
        Util(row,0)
      }
    }
    val temp1:List[List[Double]] = mixedLayer(image,kernel1,imagesize,kernel1size,(x:Double)=> if(x>0) x else 0,avg,size)
    //println(temp1)
    //println("***")
    val temp2 :List[List[Double]]= mixedLayer(image,kernel2,imagesize,kernel2size,(x:Double)=> if(x>0) x else 0,avg,size)
  //  println(temp2)
  //  println("***")
    val temp3 :List[List[Double]]= add(temp1,temp2,w1,w2,b)

    val temp3size:List[Int]= List(getRow(temp3),getCol(temp3))

    val temp4:List[List[Double]] = mixedLayer(temp3,kernel3,temp3size,kernel3size,(x:Double)=> if(x>0) x else 0.5*x,max,size)

    val norm:List[List[Int]]=normalise(temp4)

    norm
  }
}
