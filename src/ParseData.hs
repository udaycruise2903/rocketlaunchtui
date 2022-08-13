{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module ParseData where

import Data.Aeson (ToJSON(..), FromJSON(..), Value(..), (.:), (.=), object)
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)

data Configuration = Configuration
  { configurationName :: Text
  , configurationId :: Int
  , configurationVariant :: Text
  , configurationUrl :: Text
  , configurationFullName :: Text
  , configurationFamily :: Text
  } deriving (Show, Eq, Ord)

data LaunchServiceProvider = LaunchServiceProvider
  { launchServiceProviderName :: Text
  , launchServiceProviderId :: Int
  , launchServiceProviderType :: Text
  , launchServiceProviderUrl :: Text
  } deriving (Show, Eq, Ord)

data Location = Location
  { locationMapImage :: Text
  , locationName :: Text
  , locationId :: Int
  , locationTotalLandingCount :: Int
  , locationTotalLaunchCount :: Int
  , locationCountryCode :: Text
  , locationUrl :: Text
  } deriving (Show, Eq, Ord)

data Mission = Mission
  { missionName :: Text
  , missionId :: Int
  , missionType :: Text
  , missionDescription :: Text
  , missionOrbit :: Orbit
  , missionLaunchDesignator :: Maybe Value
  } deriving (Show, Eq, Ord)

data Model = Model
  { modelPrevious :: Maybe Value
  , modelNext :: Text
  , modelResults :: [Results]
  , modelCount :: Int
  } deriving (Show, Eq, Ord)

data Orbit = Orbit
  { orbitName :: Text
  , orbitId :: Int
  , orbitAbbrev :: Text
  } deriving (Show, Eq, Ord)

data Pad = Pad
  { padMapImage :: Text
  , padName :: Text
  , padId :: Int
  , padMapUrl :: Text
  , padLongitude :: Text
  , padTotalLaunchCount :: Int
  , padInfoUrl :: Maybe Value
  , padLocation :: Location
  , padAgencyId :: Maybe Value
  , padWikiUrl :: Text
  , padLatitude :: Text
  , padUrl :: Text
  } deriving (Show, Eq, Ord)

data Results = Results
  { resultsRocket :: Rocket
  , resultsMission :: Mission
  , resultsProgram :: [Value]
  , resultsWindowEnd :: Text
  , resultsName :: Text
  , resultsId :: Text
  , resultsInfographic :: Maybe Value
  , resultsLaunchServiceProvider :: LaunchServiceProvider
  , resultsStatus :: Status
  , resultsWebcastLive :: Bool
  , resultsNet :: Text
  , resultsImage :: Text
  , resultsSlug :: Text
  , resultsPad :: Pad
  , resultsProbability :: Int
  , resultsHashtag :: Maybe Value
  , resultsLastUpdated :: Text
  , resultsUrl :: Text
  , resultsHoldreason :: Text
  , resultsWindowStart :: Text
  , resultsFailreason :: Text
  } deriving (Show, Eq, Ord)

data Rocket = Rocket
  { rocketId :: Int
  , rocketConfiguration :: Configuration
  } deriving (Show, Eq, Ord)

data Status = Status
  { statusName :: Text
  , statusId :: Int
  , statusDescription :: Text
  , statusAbbrev :: Text
  } deriving (Show, Eq, Ord)

instance ToJSON Configuration where
  toJSON Configuration{..} = object
    [ "name" .= configurationName
    , "id" .= configurationId
    , "variant" .= configurationVariant
    , "url" .= configurationUrl
    , "full_name" .= configurationFullName
    , "family" .= configurationFamily
    ] 

instance ToJSON LaunchServiceProvider where
  toJSON LaunchServiceProvider{..} = object
    [ "name" .= launchServiceProviderName
    , "id" .= launchServiceProviderId
    , "type" .= launchServiceProviderType
    , "url" .= launchServiceProviderUrl
    ] 

instance ToJSON Location where
  toJSON Location{..} = object
    [ "map_image" .= locationMapImage
    , "name" .= locationName
    , "id" .= locationId
    , "total_landing_count" .= locationTotalLandingCount
    , "total_launch_count" .= locationTotalLaunchCount
    , "country_code" .= locationCountryCode
    , "url" .= locationUrl
    ] 

instance ToJSON Mission where
  toJSON Mission{..} = object
    [ "name" .= missionName
    , "id" .= missionId
    , "type" .= missionType
    , "description" .= missionDescription
    , "orbit" .= missionOrbit
    , "launch_designator" .= missionLaunchDesignator
    ] 

instance ToJSON Model where
  toJSON Model{..} = object
    [ "previous" .= modelPrevious
    , "next" .= modelNext
    , "results" .= modelResults
    , "count" .= modelCount
    ] 

instance ToJSON Orbit where
  toJSON Orbit{..} = object
    [ "name" .= orbitName
    , "id" .= orbitId
    , "abbrev" .= orbitAbbrev
    ] 

instance ToJSON Pad where
  toJSON Pad{..} = object
    [ "map_image" .= padMapImage
    , "name" .= padName
    , "id" .= padId
    , "map_url" .= padMapUrl
    , "longitude" .= padLongitude
    , "total_launch_count" .= padTotalLaunchCount
    , "info_url" .= padInfoUrl
    , "location" .= padLocation
    , "agency_id" .= padAgencyId
    , "wiki_url" .= padWikiUrl
    , "latitude" .= padLatitude
    , "url" .= padUrl
    ] 

instance ToJSON Results where
  toJSON Results{..} = object
    [ "rocket" .= resultsRocket
    , "mission" .= resultsMission
    , "program" .= resultsProgram
    , "window_end" .= resultsWindowEnd
    , "name" .= resultsName
    , "id" .= resultsId
    , "infographic" .= resultsInfographic
    , "launch_service_provider" .= resultsLaunchServiceProvider
    , "status" .= resultsStatus
    , "webcast_live" .= resultsWebcastLive
    , "net" .= resultsNet
    , "image" .= resultsImage
    , "slug" .= resultsSlug
    , "pad" .= resultsPad
    , "probability" .= resultsProbability
    , "hashtag" .= resultsHashtag
    , "last_updated" .= resultsLastUpdated
    , "url" .= resultsUrl
    , "holdreason" .= resultsHoldreason
    , "window_start" .= resultsWindowStart
    , "failreason" .= resultsFailreason
    ] 

instance ToJSON Rocket where
  toJSON Rocket{..} = object
    [ "id" .= rocketId
    , "configuration" .= rocketConfiguration
    ] 

instance ToJSON Status where
  toJSON Status{..} = object
    [ "name" .= statusName
    , "id" .= statusId
    , "description" .= statusDescription
    , "abbrev" .= statusAbbrev
    ] 

instance FromJSON Configuration where
  parseJSON (Object v) = do
    configurationName <- v .: "name"
    configurationId <- v .: "id"
    configurationVariant <- v .: "variant"
    configurationUrl <- v .: "url"
    configurationFullName <- v .: "full_name"
    configurationFamily <- v .: "family"
    pure $ Configuration{..}
  parseJSON invalid = do
    prependFailure "parsing Configuration failed, "
      (typeMismatch "Object" invalid)

instance FromJSON LaunchServiceProvider where
  parseJSON (Object v) = do
    launchServiceProviderName <- v .: "name"
    launchServiceProviderId <- v .: "id"
    launchServiceProviderType <- v .: "type"
    launchServiceProviderUrl <- v .: "url"
    pure $ LaunchServiceProvider{..}
  parseJSON invalid = do
    prependFailure "parsing LaunchServiceProvider failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Location where
  parseJSON (Object v) = do
    locationMapImage <- v .: "map_image"
    locationName <- v .: "name"
    locationId <- v .: "id"
    locationTotalLandingCount <- v .: "total_landing_count"
    locationTotalLaunchCount <- v .: "total_launch_count"
    locationCountryCode <- v .: "country_code"
    locationUrl <- v .: "url"
    pure $ Location{..}
  parseJSON invalid = do
    prependFailure "parsing Location failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Mission where
  parseJSON (Object v) = do
    missionName <- v .: "name"
    missionId <- v .: "id"
    missionType <- v .: "type"
    missionDescription <- v .: "description"
    missionOrbit <- v .: "orbit"
    missionLaunchDesignator <- v .: "launch_designator"
    pure $ Mission{..}
  parseJSON invalid = do
    prependFailure "parsing Mission failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelPrevious <- v .: "previous"
    modelNext <- v .: "next"
    modelResults <- v .: "results"
    modelCount <- v .: "count"
    pure $ Model{..}
  parseJSON invalid = do
    prependFailure "parsing Model failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Orbit where
  parseJSON (Object v) = do
    orbitName <- v .: "name"
    orbitId <- v .: "id"
    orbitAbbrev <- v .: "abbrev"
    pure $ Orbit{..}
  parseJSON invalid = do
    prependFailure "parsing Orbit failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Pad where
  parseJSON (Object v) = do
    padMapImage <- v .: "map_image"
    padName <- v .: "name"
    padId <- v .: "id"
    padMapUrl <- v .: "map_url"
    padLongitude <- v .: "longitude"
    padTotalLaunchCount <- v .: "total_launch_count"
    padInfoUrl <- v .: "info_url"
    padLocation <- v .: "location"
    padAgencyId <- v .: "agency_id"
    padWikiUrl <- v .: "wiki_url"
    padLatitude <- v .: "latitude"
    padUrl <- v .: "url"
    pure $ Pad{..}
  parseJSON invalid = do
    prependFailure "parsing Pad failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Results where
  parseJSON (Object v) = do
    resultsRocket <- v .: "rocket"
    resultsMission <- v .: "mission"
    resultsProgram <- v .: "program"
    resultsWindowEnd <- v .: "window_end"
    resultsName <- v .: "name"
    resultsId <- v .: "id"
    resultsInfographic <- v .: "infographic"
    resultsLaunchServiceProvider <- v .: "launch_service_provider"
    resultsStatus <- v .: "status"
    resultsWebcastLive <- v .: "webcast_live"
    resultsNet <- v .: "net"
    resultsImage <- v .: "image"
    resultsSlug <- v .: "slug"
    resultsPad <- v .: "pad"
    resultsProbability <- v .: "probability"
    resultsHashtag <- v .: "hashtag"
    resultsLastUpdated <- v .: "last_updated"
    resultsUrl <- v .: "url"
    resultsHoldreason <- v .: "holdreason"
    resultsWindowStart <- v .: "window_start"
    resultsFailreason <- v .: "failreason"
    pure $ Results{..}
  parseJSON invalid = do
    prependFailure "parsing Results failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Rocket where
  parseJSON (Object v) = do
    rocketId <- v .: "id"
    rocketConfiguration <- v .: "configuration"
    pure $ Rocket{..}
  parseJSON invalid = do
    prependFailure "parsing Rocket failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Status where
  parseJSON (Object v) = do
    statusName <- v .: "name"
    statusId <- v .: "id"
    statusDescription <- v .: "description"
    statusAbbrev <- v .: "abbrev"
    pure $ Status{..}
  parseJSON invalid = do
    prependFailure "parsing Status failed, "
      (typeMismatch "Object" invalid)

