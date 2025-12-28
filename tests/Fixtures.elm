module Fixtures exposing
    ( exampleAlbum
    , exampleAlbums
    , exampleImageAsset
    , exampleVideoAsset
    )

{-| Test fixtures based on real Immich API responses.

These fixtures provide realistic test data derived from actual API responses
found in docs/immich-api/example-returns/.

-}

import Date
import Immich exposing (ImmichAlbum, ImmichAsset)


{-| Example image asset based on real API response (PNG image 253.png)
-}
exampleImageAsset : ImmichAsset
exampleImageAsset =
    { id = "489f87da-6c64-4287-a681-0b49b8c223b1"
    , path = "upload/library/memes/2022/2022-01-01/PNG image 253.png"
    , title = "PNG image 253.png"
    , mimeType = "image/png"
    , thumbhash = Just "bQgKH4C61Hg/lmWjdxeZV3aD9XhGfmUA"
    , fileCreatedAt = Date.fromRataDie 738155
    , fileModifiedAt = Date.fromRataDie 738974
    , fileCreatedAtString = "2021-12-31T23:10:00.000Z"
    , fileModifiedAtString = "2025-01-28T15:42:30.000Z"
    , isFavourite = False
    , isArchived = False
    , albumMembership = []
    , duration = Just "0:00:00.00000"
    }


{-| Example video asset based on real API response (video\_2025-02-23\_21-26-11.mp4)
-}
exampleVideoAsset : ImmichAsset
exampleVideoAsset =
    { id = "75678c1d-d5ac-4f8a-8680-76ea722c5c0b"
    , path = "upload/library/memes/2025/2025-02-21/video_2025-02-23_21-26-11.mp4"
    , title = "video_2025-02-23_21-26-11.mp4"
    , mimeType = "video/mp4"
    , thumbhash = Just "2fcVDgCrdAh4eWd2iYlpd3aYma91+lg="
    , fileCreatedAt = Date.fromRataDie 739311
    , fileModifiedAt = Date.fromRataDie 739313
    , fileCreatedAtString = "2025-02-21T23:12:59.000Z"
    , fileModifiedAtString = "2025-02-23T20:26:11.411Z"
    , isFavourite = False
    , isArchived = False
    , albumMembership = []
    , duration = Just "00:01:45.557"
    }


{-| Example album based on real API response (Denmark album)
-}
exampleAlbum : ImmichAlbum
exampleAlbum =
    { id = "c56b9118-5e3a-4bbf-be9b-7540c0ea2e0a"
    , albumName = "Denmark"
    , assetCount = 3
    , assets = []
    , createdAt = Date.fromRataDie 738970
    }


{-| List of example albums based on real API responses
-}
exampleAlbums : List ImmichAlbum
exampleAlbums =
    [ { id = "c56b9118-5e3a-4bbf-be9b-7540c0ea2e0a"
      , albumName = "Denmark"
      , assetCount = 3
      , assets = []
      , createdAt = Date.fromRataDie 738970
      }
    , { id = "535b29b0-ca77-4655-9591-fa86253a0576"
      , albumName = "Atheists"
      , assetCount = 2
      , assets = []
      , createdAt = Date.fromRataDie 738970
      }
    , { id = "4383d53d-5fc5-4ec2-8b96-81b681fd124f"
      , albumName = "Art"
      , assetCount = 2
      , assets = []
      , createdAt = Date.fromRataDie 738969
      }
    , { id = "e7d18826-a8e9-4224-81ca-8c11417c3699"
      , albumName = "ToBeSorted"
      , assetCount = 4119
      , assets = []
      , createdAt = Date.fromRataDie 738964
      }
    , { id = "4f9c1a89-0ab4-4c7e-9c8e-4a92069c2389"
      , albumName = "Chess"
      , assetCount = 3
      , assets = []
      , createdAt = Date.fromRataDie 738964
      }
    ]
