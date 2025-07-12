module DebugCool exposing (..)

import Debug
import Dict
import Expect
import Test exposing (..)
import KeybindingGenerator exposing (..)
import Immich exposing (ImmichAlbum)
import Date


-- Helper function to create test albums
createTestAlbum : String -> String -> ImmichAlbum
createTestAlbum id albumName =
    { id = id
    , albumName = albumName
    , assetCount = 0
    , assets = []
    , createdAt = Date.fromRataDie 1
    }


suite : Test
suite =
    describe "Debug cool album issue"
        [ test "Debug cool candidates" <|
            \_ ->
                let
                    candidates = generateSimpleCandidates "cool"
                    _ = Debug.log "cool candidates" candidates
                in
                Expect.pass
        
        , test "Debug cool alone gets c" <|
            \_ ->
                let
                    albums = [ createTestAlbum "1" "cool" ]
                    
                    -- Debug the smart sorting process
                    albumCandidates = List.map (\album -> (album.id, generateSimpleCandidates album.albumName)) albums
                    _ = Debug.log "albumCandidates" albumCandidates
                    sortedAlbumCandidates = smartSortAlbums albumCandidates  
                    _ = Debug.log "after smart sorting" sortedAlbumCandidates
                    
                    keybindings = generateAlbumKeybindings albums
                    _ = Debug.log "cool alone keybinding" keybindings
                in
                Expect.pass
        
        , test "Debug cool with non-conflicting albums" <|
            \_ ->
                let
                    albums = 
                        [ createTestAlbum "1" "cool"
                        , createTestAlbum "2" "Travel"
                        , createTestAlbum "3" "Music"
                        ]
                    keybindings = generateAlbumKeybindings albums
                    _ = Debug.log "cool with non-conflicting keybindings" keybindings
                in
                Expect.pass
        
        , test "Debug general alone" <|
            \_ ->
                let
                    albums = [ createTestAlbum "1" "general" ]
                    candidates = generateSimpleCandidates "general"
                    _ = Debug.log "general candidates" candidates
                    keybindings = generateAlbumKeybindings albums
                    _ = Debug.log "general alone keybindings" keybindings
                in
                Expect.pass
        ]