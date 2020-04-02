package xyz.bitml.api.serialization

import org.json4s.JsonAST.{JField, JObject, JValue}
import org.json4s.{CustomSerializer, Extraction, Formats}
import xyz.bitml.api.persistence.{MetaStorage, ParticipantStorage, State, TxStorage}

class StateSerializer extends CustomSerializer[State](format =>  (
  {
  case JObject(List(JField("participants", parts:JValue), JField("tx", txs:JValue), JField("meta", metas:JValue))) => {
    implicit val formats : Formats = org.json4s.DefaultFormats + new PartStorageSerializer + new TxStorageSerializer + new MetaStorageSerializer
    State(partdb = Extraction.extract[ParticipantStorage](parts), txdb = Extraction.extract[TxStorage](txs), metadb = Extraction.extract[MetaStorage](metas))
  }},{
  case x : State => {
    implicit val formats : Formats = org.json4s.DefaultFormats + new PartStorageSerializer + new TxStorageSerializer + new MetaStorageSerializer
    JObject(List(
      JField("participants", Extraction.decompose(x.partdb)),
      JField("tx", Extraction.decompose(x.txdb)),
      JField("meta", Extraction.decompose(x.metadb))
    ))
  }}
))
