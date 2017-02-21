package tools

import miner.{FieldIn, Mine, Value}

/**
  * Created by wojciech on 21.02.17.
  */
object ConvTool {

  case object M

  def apply(tab: List[Any], height: Int, width: Int): Option[List[List[FieldIn]]] = {
    val list: List[FieldIn] = tab.collect {
      case x: Int if x >= 0 && x < 9 => Value(x)
      case M => Mine
    }
    // TODO Additional validate
    if(list.size == height * width) Some(list.sliding(width, width).toList)
    else None
  }

}
