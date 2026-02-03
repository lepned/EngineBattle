namespace ChessLibrary

open System.Collections.Generic

/// Types for representing game trees as directed acyclic graphs (DAGs).
/// Supports transpositions and variations in chess games.
module GameGraphTypes =

    /// Unique identifier for a position node in the game graph
    type NodeId = int

    /// Unique identifier for a move edge in the game graph
    type EdgeId = int

    /// A node representing a chess position in the game graph
    type PositionNode =
        { Id: NodeId
          Hash: uint64
          Fen: string
          Parents: EdgeId list
          Children: EdgeId list }

    /// An edge representing a move between positions in the game graph
    type MoveEdge =
        { Id: EdgeId
          From: NodeId
          To: NodeId
          San: string
          Lan: string
          Comments: string
          Color: string
          IsCastling: bool
          Order: int
          IsMainline: bool }

    /// The complete game graph structure with nodes and edges
    type MoveGraph =
        { mutable Root: NodeId
          NodesById: Dictionary<NodeId, PositionNode>
          EdgesById: Dictionary<EdgeId, MoveEdge>
          NodesByHash: Dictionary<uint64, ResizeArray<NodeId>> }

    /// A token representing a move in an inline display format
    [<CLIMutable>]
    type InlineMoveToken =
        { Text: string
          DisplayText: string
          Fen: string
          MoveCoord: string
          IsBracket: bool
          Hash: uint64
          FromVariation: bool
          Ply: int
          Evaluation: string
          IsLineStart: bool }
