using System;
using System.Collections.Generic;
using System.Linq;
using ChessLibrary;

namespace WebGUI.Components.Layout.ChessboardLayout;

/// <summary>Turns the shapes a PGN comment asks for — <c>[%csl Gd4,Rf6]</c> highlighted
/// squares and <c>[%cal Ge2e4]</c> arrows — into board overlay shapes.
///
/// The colour letters are fixed by the convention (G, R, Y, B); what they should look like is
/// not. Lichess draws them in dark, saturated tones that disappear against several of EB's
/// board themes, so these are brighter equivalents chosen to hold up on light and dark
/// squares alike. Squares are drawn as rings and insights as filled discs, so a reader can
/// tell an authored annotation from a computed one on the same board.</summary>
public static class CommentShapes
{
    public const string Green = "#2FBF71";
    public const string Red = "#E5484D";
    public const string Yellow = "#F5C518";
    public const string Blue = "#3E8FF0";

    private static string ColorOf(char letter) => letter switch
    {
        'G' => Green,
        'R' => Red,
        'Y' => Yellow,
        'B' => Blue,
        _ => Green,
    };

    private static IReadOnlyList<PGNComment.CommentShape> Parse(string comment) =>
        string.IsNullOrWhiteSpace(comment)
            ? Array.Empty<PGNComment.CommentShape>()
            : PGNComment.shapes(comment).ToArray();

    /// <summary>Ring highlights from the comment's <c>[%csl …]</c> command.</summary>
    public static IReadOnlyList<EbCircle> Circles(string comment)
    {
        var shapes = Parse(comment);
        if (shapes.Count == 0) return Array.Empty<EbCircle>();
        return shapes.Where(s => !s.IsArrow)
                     .Select(s => new EbCircle(s.From, ColorOf(s.Color), Opacity: 0.95, Size: 0.92, StrokeWidth: 0.07))
                     .ToArray();
    }

    /// <summary>Arrows from the comment's <c>[%cal …]</c> command.</summary>
    public static IReadOnlyList<EbArrow> Arrows(string comment)
    {
        var shapes = Parse(comment);
        if (shapes.Count == 0) return Array.Empty<EbArrow>();
        return shapes.Where(s => s.IsArrow)
                     .Select(s => new EbArrow(s.From, s.To, ColorOf(s.Color), Opacity: 0.85))
                     .ToArray();
    }

    /// <summary>The colour letter to write for an insight shape.
    ///
    /// The convention has four letters and EB's insights have nine colours, so the
    /// collapse is lossy and has to be made on meaning. It follows the split the insight
    /// toggles already draw: the danger group separates into threats (red), the pieces
    /// those threats hang over (yellow) and the squares that escape them (green), while
    /// the whole tactics family — fork, skewer, overload, discovered attack, removable
    /// defender — reads as one idea (blue). A reader in lichess keeps "this is dangerous"
    /// apart from "this is an opportunity", which is what the insights actually say.</summary>
    private static char LetterFor(string hex) => hex switch
    {
        InsightShapes.PinColor or InsightShapes.CheckColor => 'R',
        InsightShapes.HangingColor => 'Y',
        InsightShapes.EscapeColor => 'G',
        _ => 'B',
    };

    /// <summary>The shapes currently drawn on the board, as PGN comment shapes ready for
    /// <see cref="PGNComment.formatShapes"/>. Squares come out before arrows, matching the
    /// order the writer emits.</summary>
    public static IReadOnlyList<PGNComment.CommentShape> FromBoard(
        IReadOnlyList<EbArrow> arrows, IReadOnlyList<EbCircle> circles)
    {
        var shapes = new List<PGNComment.CommentShape>();
        foreach (var c in circles ?? Array.Empty<EbCircle>())
            shapes.Add(new PGNComment.CommentShape(LetterFor(c.Color), c.Square, ""));
        foreach (var a in arrows ?? Array.Empty<EbArrow>())
            shapes.Add(new PGNComment.CommentShape(LetterFor(a.Color), a.From, a.To));
        return shapes;
    }
}
