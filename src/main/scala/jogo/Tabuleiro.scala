package jogo

import org.scalajs.dom
import org.scalajs.dom.html
import dom.document

import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import scalatags.JsDom.all.{button, _}

import scala.util.Random
import scala.scalajs.js.timers._

@JSExport
object Tabuleiro {

  val divisao = document.getElementById("div").asInstanceOf[html.Div]
  val corpo = dom.document
  var pontuacao = 0
  var recorde = 0

  val COR_BLOCO = "#B0C4DE"
  val COR_PLANO_DE_FUNDO = "#778899"

  val DIM: Int = 3
  var matriz : Array[Array[Int]] = Array.ofDim[Int](DIM + 1, DIM + 1)
  val posSomadas: Array[Array[Boolean]] = Array.ofDim[Boolean](DIM + 1, DIM + 1)

  @JSExport
  def jogar(): Unit = {

    addProx()
    addProx()
    plano_de_fundo()
    atualizarValores('0')
    mensagemPont()
    mensagemRecorde()
    mensagemFimJogo()
  }

  def addProx(i: Int = rand(), j: Int = rand(), valor: Int = next()): Unit = {
    if (posVazias()) {
      if (matriz(i)(j) == 0) {
        matriz(i)(j) = valor
      } else {
        addProx()
      }
    }
  }

  def next(): Int = {
    val random = new Random()
    if (random.nextFloat() < 0.75f) 2 else 4
  }

  def rand(): Int = {
    val random = new Random()
    random.nextInt(4)
  }

  def posVazias(): Boolean = {
    def aux1(i: Int): Boolean = {
      def aux2(j: Int): Boolean = {
        j match {
          case j if j > DIM => false
          case _ => if (matriz(i)(j) == 0) true else aux2(j + 1)
        }
      }

      i match {
        case i if i > DIM => false
        case _ => if (aux2(0)) true else aux1(i + 1)
      }
    }

    aux1(0)
  }

  def tabCompleto(): Boolean = {
    def for1(x: Int): Boolean = {
      def for2(y: Int): Boolean = {
        y match {
          case y if y > DIM => for1(x + 1)
          case _ => if (matriz(x)(y) == 0) false else for2(y + 1)
        }
      }

      x match {
        case x if x > DIM => true
        case _ => for2(0)
      }
    }

    for1(0)
  }

  def existemSomas(x: Int, y: Int): Boolean = {
    if (x - 1 >= 0) {
      if (matriz(x - 1)(y) == matriz(x)(y)) {
        return true
      }
    }
    if (x + 1 <= DIM) {
      if (matriz(x + 1)(y) == matriz(x)(y)) {
        return true
      }
    }
    if (y + 1 <= DIM) {
      if (matriz(x)(y + 1) == matriz(x)(y)) {
        return true
      }
    }
    if (y - 1 >= 0) {

      if (matriz(x)(y - 1) == matriz(x)(y)) {
        return true
      }

    }
    false
  }

  def fimJogo(): Boolean = {
    def for1(i: Int): Boolean = {
      def for2(j: Int): Boolean = {
        j match {
          case j if j > DIM => for1(i + 1)
          case _ => if (existemSomas(i, j)) {
            false
          } else for2(j + 1)
        }
      }

      i match {
        case i if i > DIM => true
        case _ => for2(0)
      }
    }

    if (tabCompleto()) {
      for1(0)
    } else {
      false
    }
  }

  def movimento(sentido: Int): Boolean = {
    val matrizAux = clona(matriz)

    if (sentido == 38) { // MOVIMENTO PARA CIMA
      movimento_cima(1, 0)
      movimento_cima(1, 1)
      movimento_cima(1, 2)
      movimento_cima(1, 3)
    } else if (sentido == 40) { // MOVIMENTO PARA BAIXO
      movimento_baixo(2, 0)
      movimento_baixo(2, 1)
      movimento_baixo(2, 2)
      movimento_baixo(2, 3)
    } else if (sentido == 37) { // MOVIMENTO PARA ESQUERDA
      movimento_esquerda(0, 1)
      movimento_esquerda(1, 1)
      movimento_esquerda(2, 1)
      movimento_esquerda(3, 1)
    } else if (sentido == 39) { // MOVIMENTO PARA DIREITA
      movimento_direita(0, 2)
      movimento_direita(1, 2)
      movimento_direita(2, 2)
      movimento_direita(3, 2)
    }
    limparSomas()

    if(saoIguais(matrizAux,matriz)){
      false
    }else{
      true
    }

  }

  def movimento_direita(x: Int, y: Int): Unit = {
    if (y >= 0) {
      if (matriz(x)(y) != 0) {
        val newPos = avaliar_movimentoY_direita(x, y + 1)

        if (newPos != (x, y)) {
          matriz(newPos._1)(newPos._2) = matriz(x)(y)
          matriz(x)(y) = 0
        }

        if (newPos._2 + 1 <= DIM) // TENTAR SOMAR (X,Y) COM (X, Y + 1)
          soma(newPos._1, newPos._2, newPos._1, newPos._2 + 1)
      }
      movimento_direita(x, y - 1)
    }
  }

  def avaliar_movimentoY_esquerda(x: Int, y: Int): (Int, Int) = {
    (x, y) match {
      case (x, 0) => if (matriz(x)(y) == 0) (x, 0) else (x, y + 1)
      case (x, y) => {
        if (matriz(x)(y) == 0) {
          val avMov = avaliar_movimentoY_esquerda(x, y - 1)

          if (avMov != (x, y)) {
            avMov
          } else {
            (x, y)
          }
        } else {
          (x, y + 1)
        }
      }
    }
  }

  def movimento_esquerda(x: Int, y: Int): Unit = {
    if (y <= DIM) {
      if (matriz(x)(y) != 0) {
        val newPos = avaliar_movimentoY_esquerda(x, y - 1)

        if (newPos != (x, y)) {
          matriz(newPos._1)(newPos._2) = matriz(x)(y)
          matriz(x)(y) = 0
        }

        if (newPos._2 - 1 >= 0) // TENTAR SOMAR (X,Y) COM (X, Y - 1)
          soma(newPos._1, newPos._2, newPos._1, newPos._2 - 1)
      }

      movimento_esquerda(x, y + 1)
    }
  }

  def movimento_baixo(x: Int, y: Int): Unit = {
    if (x >= 0) {
      if (matriz(x)(y) != 0) {
        val newPos = avaliar_movimentoX_baixo(x + 1, y)

        if (newPos != (x, y)) {
          matriz(newPos._1)(newPos._2) = matriz(x)(y)
          matriz(x)(y) = 0
        }

        if (newPos._1 + 1 <= DIM) // TENTAR SOMAR (X,Y) COM (X + 1, Y)
          soma(newPos._1, newPos._2, newPos._1 + 1, newPos._2)

      }

      movimento_baixo(x - 1, y)
    }
  }

  def movimento_cima(x: Int, y: Int): Unit = {
    if (x <= DIM) {
      if (matriz(x)(y) != 0) {
        val newPos = avaliar_movimentoX_cima(x - 1, y)

        if (newPos != (x, y)) {
          matriz(newPos._1)(newPos._2) = matriz(x)(y)
          // TENTAR SOMAR (X,Y) COM (X, Y + 1)
          matriz(x)(y) = 0
        }

        if (newPos._1 - 1 >= 0) // TENTAR SOMAR (X,Y) COM (X - 1, Y)
          soma(newPos._1, newPos._2, newPos._1 - 1, newPos._2)

      }

      movimento_cima(x + 1, y)
    }
  }

  def avaliar_movimentoY_direita(x: Int, y: Int): (Int, Int) = {
    (x, y) match {
      case (x, DIM) => if (matriz(x)(y) == 0) (x, y) else (x, y - 1)
      case (x, y) => {
        if (matriz(x)(y) == 0) {
          val avMov = avaliar_movimentoY_direita(x, y + 1)

          if (avMov != (x, y)) {
            avMov
          } else {
            (x, y)
          }
        } else {
          (x, y - 1)
        }
      }
    }
  }

  def avaliar_movimentoX_baixo(x: Int, y: Int): (Int, Int) = {
    (x, y) match {
      case (DIM, y) => if (matriz(x)(y) == 0) (x, y) else (x - 1, y)
      case (x, y) => {
        if (matriz(x)(y) == 0) {
          val avMov = avaliar_movimentoX_baixo(x + 1, y)

          if (avMov != (x, y)) {
            avMov
          } else {
            (x, y)
          }
        } else {
          (x - 1, y)
        }
      }
    }
  }

  def avaliar_movimentoX_cima(x: Int, y: Int): (Int, Int) = {
    (x, y) match {
      case (0, y) => if (matriz(x)(y) == 0) (x, y) else (x + 1, y)
      case (x, y) => {
        if (matriz(x)(y) == 0) {
          val avMov = avaliar_movimentoX_cima(x - 1, y)

          if (avMov != (x, y)) {
            avMov
          } else {
            (x, y)
          }
        } else {
          (x + 1, y)
        }
      }
    }
  }

  def soma(x: Int, y: Int, toX: Int, toY: Int): Unit = {
    if (matriz(x)(y) == matriz(toX)(toY)) {
      if (!posSomadas(x)(y)) {
        matriz(toX)(toY) += matriz(x)(y)
        pontuacao += matriz(toX)(toY)
        matriz(x)(y) = 0
        setPosicoesSomadas(x, y)
      }
    }
  }

  def setPosicoesSomadas(x: Int, y: Int): Unit = {
    posSomadas(x)(y) = true
  }

  def limparSomas(): Unit = {
    def limpar (x : Int) : Boolean = {
      x match {
        case 4 => true
        case _ =>{
          posSomadas.update(x, Array.ofDim[Boolean](DIM + 1))
          limpar(x + 1)
        }
      }
    }
    limpar(0)
  }

  def saoIguais(matriz1:Array[Array[Int]],matriz2:Array[Array[Int]]):Boolean = {
    def for1(i:Int):Boolean ={
      def for2(j:Int):Boolean = {
        j match {
          case j if j > DIM => for1(i+1)
          case _ => if(matriz1(i)(j) == matriz2(i)(j)) for2(j+1) else false
        }
      }
      i match{
        case i if i > DIM => true
        case _ => for2(0)
      }
    }
    for1(0)
  }

  def clona(matriz:Array[Array[Int]]):Array[Array[Int]] = {
    val matrizAux = Array.ofDim[Int](DIM+1,DIM+1)
    for(i <- 0 to DIM){
      for(j <- 0 to DIM){
        matrizAux(i)(j) = matriz(i)(j)
      }
    }
    matrizAux
  }

  def plano_de_fundo() = {
    val pndFundo = div().render

    pndFundo.style_=("Background-color: " + COR_PLANO_DE_FUNDO) // ALTERA A COR DA DIV - PLANO DE FUNDO
    pndFundo.className = "pndFundo"
    pndFundo.id = "pndFundo"

    // ADICIONA A 1a LINHA COM OS 4 BLOCOS
    pndFundo.appendChild(
      div(`class` := "linhaBlocos")(
        bloco_vazio(1).render,
        bloco_vazio(2).render,
        bloco_vazio(3).render,
        bloco_vazio(4).render
      ).render
    )

    // ADICIONA A 2a LINHA COM OS 4 BLOCOS
    pndFundo.appendChild(
      div(`class` := "linhaBlocos")(
        bloco_vazio(5).render,
        bloco_vazio(6).render,
        bloco_vazio(7).render,
        bloco_vazio(8).render
      ).render
    )

    // ADICIONA A 3a LINHA COM OS 4 BLOCOS
    pndFundo.appendChild(
      div(`class` := "linhaBlocos")(
        bloco_vazio(9).render,
        bloco_vazio(10).render,
        bloco_vazio(11).render,
        bloco_vazio(12).render
      ).render
    )

    // ADICIONA A 4a LINHA COM OS 4 BLOCOS
    pndFundo.appendChild(
      div(`class` := "linhaBlocos")(
        bloco_vazio(13).render,
        bloco_vazio(14).render,
        bloco_vazio(15).render,
        bloco_vazio(16).render
      ).render
    )


    corpo.body.appendChild(
      pndFundo.render
    )
  }

  def bloco_vazio(num: Int): html.Div = {
    // CRIA UM NOVO BLOCO COM UMA LABEL QUE ARMAZENARA O VALOR DO BLOCO
    val bloco = div(id:="bloco"+num, `class` := "bloco")(
      label(id := "valor" + num, `class` := "valor")(
      ) // num SERVIRA PARA LOCALIZAR A LABEL MAIS FACILMENTE
    ).render

    // ALTERA A COR DO BLOCO
    bloco.style_=("background-color: " + COR_BLOCO + ";")

    // RETORNA O BLOCO CRIADO
    bloco
  }

  def tamanhoDoNumero(num: Int): Double = {
    15 - (("" + num).length - 1) * 2.5
  }

  def atualizarValores(sentido:Int) = {
    val valores = corpo.getElementsByClassName("linhaBlocos")
    def preencheTab(x : Int, y : Int) : Boolean = {
      x match {
        case 4 => true
        case _ => {
          y match {
            case 4 => true
            case _ => {
              if (matriz(x)(y) != 0) {
                valores(x).getElementsByClassName("valor")(y).textContent = "" + matriz(x)(y)
                valores(x).getElementsByClassName("valor")(y).
                  asInstanceOf[html.Label].style = ("font-size: " + tamanhoDoNumero(matriz(x)(y)) + "vmin;")
              } else {
                valores(x).getElementsByClassName("valor")(y).textContent = ""
                valores(x).getElementsByClassName("bloco")(y).asInstanceOf[html.Div].
                  style_=("background-color: " + COR_BLOCO + ";")
              }
              preencheTab(x, y + 1)
            }
          }
          preencheTab(x + 1, 0)
        }
      }
    }

    if (sentido == 37 || sentido == 38 || sentido == 39 || sentido == 40) {
      if (movimento(sentido)) {
        setTimeout(100) {
          addProx()
          preencheTab(0,0)
          if(fimJogo()){
            val painelFimJogo = corpo.getElementById("painelFimJogo")
            painelFimJogo.asInstanceOf[html.Div].style = ("display: block;")
            val lblPontuacao = corpo.getElementById("lblPontuacao")
            lblPontuacao.textContent = "Sua pontuação foi: " + pontuacao
          }
        }

        if(pontuacao > recorde){
          recorde = pontuacao
        }
      }
    } else {
      preencheTab(0,0)
    }
  }

  @JSExport
  def reiniciar() = {
    matriz = Array.ofDim[Int](DIM + 1, DIM + 1)

    val painelFimJogo = corpo.getElementById("painelFimJogo")
    painelFimJogo.asInstanceOf[html.Div].style = ("display: none;")

    pontuacao = 0

    addProx()
    addProx()
    atualizarPontuacaoERecorde()
    atualizarValores('0')
  }

  @JSExport
  def finalizar() = {
    matriz = Array.ofDim[Int](DIM + 1, DIM + 1)

    val painelFimJogo = corpo.getElementById("painelFimJogo")
    painelFimJogo.parentNode.removeChild(painelFimJogo)
    pontuacao = 0

    corpo.location.href = "http://localhost:63342/doismil/Menu.html?_ijt=saoon0ks5i9tp2352bd6ub89dn"
  }

  def mensagemFimJogo() = {
    val container = div(id:="painelFimJogo", style:="display:none;")(
      label(id:="lblFimJogo", `class`:="clslblFimJogo")("Fim de Jogo").render,
      label(id:="lblPontuacao", `class`:="clslblFimJogo")("Sua pontuação foi: " + pontuacao).render,
      button(id:="reiniciar", onclick:="jogo.Tabuleiro().reiniciar()", `class`:="clsbtnFimJogo")("Reiniciar").render,
      button(id:="finalizar", onclick:="jogo.Tabuleiro().finalizar()", `class`:="clsbtnFimJogo")("Finalizar").render
    )

    corpo.body.appendChild(
      container.render
    )
  }

  def mensagemPont() ={

    val cont = div(id:="painelPont")(
      label(id:="lblPont", `class`:="clslblPont")("Pontuação: " + pontuacao).render
    )

    corpo.body.appendChild(
      cont.render
    )
  }

  def atualizarPontuacaoERecorde() = {
    val lblPontuacao = corpo.getElementById("lblPont").asInstanceOf[html.Label]
    val lblRecorde = corpo.getElementById("lblRecorde").asInstanceOf[html.Label]

    lblPontuacao.textContent = "Pontuação: " + pontuacao
    lblRecorde.textContent = "Recorde: " + recorde
  }

  @JSExport
  def atualizarJogo(sentido:Int) = {
    atualizarValores(sentido)
    atualizarPontuacaoERecorde()
  }

  def mensagemRecorde() ={

    val cont = div(id:="painelRecorde")(
      label(id:="lblRecorde", `class`:="clslblRecorde")("Recorde: " + recorde).render
    )

    corpo.body.appendChild(
      cont.render
    )
  }

}
