Module MyBot

    Class MyBot
        Implements Bot

        Public Sub DoSetup(ByVal a As Ants) Implements Bot.DoSetup
        End Sub

        Public Sub DoTurn(ByVal a As Ants) Implements Bot.DoTurn
            For Each antLoc In a.MyAnts()
                Dim directions() As Direction = {Direction.n, Direction.e, Direction.s, Direction.w}
                For Each d In directions
                    Dim newLoc = a.Destination(antLoc, d)
                    If a.Passable(newLoc) Then
                        a.IssueOrder(antLoc, d)
                        Exit For
                    End If
                Next
                If a.TimeRemaining < 10 Then
                    Exit For
                End If
            Next
        End Sub
    End Class

    Sub Main()
        Ants.Run(New MyBot())
    End Sub

End Module
