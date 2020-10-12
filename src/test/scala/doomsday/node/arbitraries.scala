package doomsday.node

import org.scalacheck.Arbitrary
import org.scalacheck.Gen
import doomsday.state.CRDTState
import doomsday.state.CRDTOperation

object arbitraries {
  def genMsgSeq[StateT, OpT, MsgT](implicit
      node: Gen[Node[StateT, OpT, MsgT]],
      op: Arbitrary[OpT]
  ): Gen[Seq[MsgT]] =
    for {
      node <- node
      op <- op.arbitrary
    } yield node.messages(op)

  def genInterleaveMsgSeqs[MsgT](sequences: Seq[Seq[MsgT]]): Gen[Seq[MsgT]] =
    sequences match {
      case Nil => Gen.const(Seq.empty)
      case sequences =>
        val seqs = sequences.filterNot(_.isEmpty)
        for {
          seqId <- Gen.choose(0, seqs.size - 1)
          tail <- genInterleaveMsgSeqs(seqs.updated(seqId, seqs(seqId).tail))
        } yield seqs(seqId).head +: tail
    }

  implicit def arbMsgSeq[StateT, OpT, MsgT](implicit
      node: Gen[Node[StateT, OpT, MsgT]],
      op: Arbitrary[OpT]
  ) =
    Arbitrary(
      for {
        numOfNodes <- Gen.choose(1, 5)
        msgSeqs <- Gen.listOfN(numOfNodes, genMsgSeq)
        msgSeq <- genInterleaveMsgSeqs(msgSeqs)
      } yield msgSeq
    )

  def genNode[StateT, OpT, MsgT](implicit
      ops: Arbitrary[OpT],
      st: CRDTState[StateT],
      o: CRDTOperation[StateT, OpT, MsgT]
  ): Gen[Node[StateT, OpT, MsgT]] =
    Gen.frequency(
      (1, for { nodeId <- Gen.identifier } yield Node.empty(nodeId)),
      (
        49,
        for {
          op <- ops.arbitrary
          node <- genNode(ops, st, o)
        } yield node.newState(op)
      )
    )

  implicit def arbNode[StateT, OpT, MsgT](implicit
      ops: Arbitrary[OpT],
      st: CRDTState[StateT],
      o: CRDTOperation[StateT, OpT, MsgT]
  ) = Arbitrary(genNode(ops, st, o))

  implicit def arbState[StateT, OpT, MsgT](implicit nodes: Arbitrary[Node[StateT, OpT, MsgT]]) =
    Arbitrary(
      for {
        node <- nodes.arbitrary
      } yield node.state
    )
}
