package doomsday.node

import doomsday.state.CRDTGetValue
import doomsday.state.CRDTApplyMsg
import cats.implicits._
import org.typelevel.discipline.Laws
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import doomsday.state.CRDTState
import doomsday.state.CRDTOperation

case class CRDTLaws[StateT, MsgT, OpT, ValT]()(implicit
    S: CRDTState[StateT],
    A: CRDTApplyMsg[StateT, MsgT],
    V: CRDTGetValue[StateT, ValT],
    O: CRDTOperation[StateT, OpT, MsgT],
    arbOp: Arbitrary[OpT]
) extends Laws {

  implicit def arbNode: Arbitrary[Node[StateT, OpT, MsgT]] = Arbitrary(genNode)

  implicit def arbState: Arbitrary[StateT] =
    Arbitrary(
      for {
        node <- genNode
      } yield node.state
    )

  implicit def arbMsgSeqPermutations: Arbitrary[Seq[Seq[MsgT]]] =
    Arbitrary(
      for {
        numNodes <- Gen.choose(1, 5)
        msgSeqs <- Gen.listOfN(numNodes, genMsgSeq)
        numPermutations <- Gen.choose(1, 20)
        msgSeqPermutations <- Gen.listOfN(numPermutations, genInterleaveMsgSeqs(msgSeqs))
      } yield msgSeqPermutations
    )

  def genMsgSeq: Gen[Seq[MsgT]] =
    for {
      node <- genNode
      numOps <- Gen.choose(0, 50)
      ops <- Gen.listOfN(numOps, arbOp.arbitrary)
    } yield ops
      .foldLeft((node, Seq.empty[MsgT])) {
        case ((node, msgs), op) => (node.newState(op), msgs ++ node.messages(op))
      }
      ._2

  def genInterleaveMsgSeqs(sequences: Seq[Seq[MsgT]]): Gen[Seq[MsgT]] =
    sequences.filterNot(_.isEmpty) match {
      case Nil => Gen.const(Seq.empty)
      case sequences =>
        for {
          seqId <- Gen.choose(0, sequences.size - 1)
          tail <- genInterleaveMsgSeqs(sequences.updated(seqId, sequences(seqId).tail))
        } yield sequences(seqId).head +: tail
    }

  def genNode: Gen[Node[StateT, OpT, MsgT]] =
    Gen.frequency(
      (1, for { nodeId <- Gen.identifier } yield Node.empty(nodeId)(S, O)),
      (
        49,
        for {
          op <- arbOp.arbitrary
          node <- genNode
        } yield node.newState(op)
      )
    )

  private def operationChangesValues(x: Node[StateT, OpT, MsgT], op: OpT) =
    V.getValues(x.messages(op).foldLeft(x.newState(op).state) { case (state, msg) => A.update(state, msg) }) =!= V
      .getValues(x.state)

  private def operationSetsNewTime(x: Node[StateT, OpT, MsgT], op: OpT) =
    V.getTimes(x.messages(op).foldLeft(x.newState(op).state) { case (state, msg) => A.update(state, msg) }) === Set(
      x.clock.inc(x.nodeId)
    )

  private def messageSequencesConverge(x: StateT, msgs: Seq[Seq[MsgT]]) = {
    val values = msgs.map(_.foldLeft(x) { case (state, msg) => A.update(state, msg) }).map(V.getValues _)
    values.forall(_ === values.head)
  }

  def crdt: RuleSet =
    new RuleSet {
      override def name: String = "CRDTgetValue"
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      def parents = Seq.empty
      def props =
        Seq(
          "operationChangesValues" -> forAll(operationChangesValues _),
          "operationSetsNewTime" -> forAll(operationSetsNewTime _),
          "messageSequencesConverge" -> forAll(messageSequencesConverge _)
        )
    }
}
