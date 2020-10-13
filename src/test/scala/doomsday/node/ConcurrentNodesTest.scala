package doomsday.node

import org.typelevel.discipline.Laws
import org.scalacheck.Gen
import doomsday.state.CRDTState
import doomsday.state.CRDTOperation
import doomsday.state.CRDTApplyMsg
import doomsday.state.CRDTGetValue
import org.scalacheck.Arbitrary
import org.scalacheck.Prop.forAll

// Simulates a stateful network of a number of nodes that concurrently process operations and send messages
case class ConcurrentNodesTest[StateT, MsgT, OpT, ValT]()(implicit
    S: CRDTState[StateT],
    A: CRDTApplyMsg[StateT, MsgT],
    V: CRDTGetValue[StateT, ValT],
    O: CRDTOperation[StateT, OpT, MsgT],
    arbOp: Arbitrary[OpT]
) extends Laws {

  // Models a network of nodes, that execute operations/messages and send messages to each other via a buffer
  case class Network(
      nodes: Seq[Node[StateT, OpT, MsgT]],
      operations: Seq[Seq[OpT]],
      channels: Map[(Int, Int), Seq[MsgT]]
  ) {
    val numNodes = nodes.size
    val isFinished = operations.forall(_.isEmpty) && channels.forall { case (_ -> seq) => seq.isEmpty }

    def possibleOperations = operations.zipWithIndex.filter(_._1.nonEmpty).map(_._2)
    def possibleReceives = channels.filter { case (_) -> seq => seq.nonEmpty }.keySet

    // Executes a state transfer in which node node processes an operation
    def doOperation(node: Int): Network = {
      if (operations(node).isEmpty) this
      else {
        val updatedNodes = nodes.updated(node, nodes(node).newState(operations(node).head))
        val updatedOperations = operations.updated(node, operations(node).tail)
        val updatedChannels = channels.map {
          case ((from, to) -> msgSeq) if from == node =>
            ((from, to) -> (msgSeq ++ nodes(node).messages(operations(node).head)))
          case other => other
        }
        Network(updatedNodes, updatedOperations, updatedChannels)
      }
    }

    // Executes a state transfer in which node to receives an processes a message from node from
    def doReceive(from: Int, to: Int): Network = {
      if (to >= nodes.size) {
        println(this)
        println(possibleReceives)
      }
      if (channels((from, to)).isEmpty) this
      else {
        val updatedNodes = nodes.updated(to, nodes(to).processMsg(channels((from, to)).head))
        val updatedOperations = operations
        val updatedChannels = channels + (((from, to)) -> channels((from, to)).tail)
        Network(updatedNodes, updatedOperations, updatedChannels)
      }
    }

    // Shrink network to nodes contained in keep
    def subnetwork(keep: Set[Int]): Network = {
      /*val mapping = nodes.indices.flatMap(index =>
        keep.toSeq.sorted.indexOf(index) match {
          case -1 => Seq.empty
          case n  => Seq(n)
        }
      )*/
      val updatedNodes = nodes.zipWithIndex.filter(node => keep.contains(node._2)).map(_._1)
      val updatedOperations = operations.zipWithIndex.filter(node => keep.contains(node._2)).map(_._1)
      val updatedChannels = channels
        .filter {
          case ((from, to) -> _) if !keep.contains(from) || !keep.contains(to) => false
          case _                                                               => true
        }
        .map {
          case ((from, to) -> seq) => ((keep.toSeq.sorted.indexOf(from), keep.toSeq.sorted.indexOf(to)) -> seq)
        }
        .toMap
      Network(updatedNodes, updatedOperations, updatedChannels)
    }
  }

  implicit def arbNetwork: Arbitrary[Network] =
    Arbitrary(
      genNetwork
    )

  implicit def arbNetworkMsgPermutations: Arbitrary[Seq[Network]] =
    Arbitrary(
      for {
        network <- genNetwork
        permutations <- Gen.listOfN(2, processRemainingMessages(network))
      } yield permutations
    )

  def processRemainingMessages(network: Network): Gen[Network] =
    if (network.possibleReceives.isEmpty) Gen.const(network)
    else
      for {
        (from, to) <- Gen.oneOf(network.possibleReceives)
        newNetwork <- processRemainingMessages(network.doReceive(from, to))
      } yield newNetwork

  def genNetwork: Gen[Network] =
    for {
      iterations <- Gen.choose(0, 50)
      network <- (0 until iterations).foldRight(genFreshNetwork) { case (_, network) => genTransformNetwork(network) }
    } yield network

  def genTransformNetwork(network: Gen[Network]) =
    Gen.frequency(
      (1, genNodeFail(network)),
      (10, genExecOperation(network)),
      (89, genReceiveMessage(network))
    )

  def genReceiveMessage(network: Gen[Network]): Gen[Network] =
    network.flatMap(nw =>
      if (nw.possibleReceives.isEmpty) network
      else
        for {
          (from, to) <- Gen.oneOf(nw.possibleReceives)
        } yield nw.doReceive(from, to)
    )

  def genExecOperation(network: Gen[Network]): Gen[Network] =
    network.flatMap(nw =>
      if (nw.possibleOperations.isEmpty) network
      else
        for {
          node <- Gen.oneOf(nw.possibleOperations)
        } yield nw.doOperation(node)
    )

  def genNodeFail(network: Gen[Network]): Gen[Network] =
    for {
      nw <- network
      node <- Gen.oneOf(0 until nw.nodes.size)
    } yield if (nw.numNodes > 2) nw.subnetwork((0 until nw.nodes.size).toSet - node) else nw

  def genFreshNetwork: Gen[Network] =
    for {
      numNodes <- Gen.choose(2, 5)
      nodes <- Gen.listOfN(numNodes, genEmptyNode)
      operations <- Gen.listOfN(numNodes, Gen.infiniteStream(arbOp.arbitrary))
    } yield Network(
      nodes,
      operations,
      Map.from(for {
        from <- 0 until nodes.size
        to <- 0 until nodes.size
      } yield (from, to) -> Seq.empty[MsgT])
    )

  def genEmptyNode: Gen[Node[StateT, OpT, MsgT]] =
    for {
      nodeId <- Gen.identifier
    } yield Node.empty(nodeId)

  private def networkPermutationsConverges(x: Seq[Network]) = {
    val values = x.flatMap(_.nodes).map(_.state).map(V.getUniqueValue)
    values.forall(_ == values.head)
  }

  def crdt: RuleSet =
    new RuleSet {
      override def name: String = "ConcurrentNodesModel"
      def bases: Seq[(String, Laws#RuleSet)] = Seq.empty
      def parents = Seq.empty
      def props =
        Seq(
          "convergence after all messages are processed" -> forAll(networkPermutationsConverges _)
        )
    }
}
