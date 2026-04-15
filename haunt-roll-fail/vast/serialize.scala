// FILE: vast/serialize.scala
// VERSION: 0.1.0
// START_MODULE_CONTRACT
// PURPOSE: Bind Vast to the shared serializer framework through faction parsing and a module-specific prefix.
// SCOPE: Delegate faction read/write operations to Meta and expose the Vast serializer prefix.
// DEPENDS: vast.Meta, hrf.serialize.Serializer
// LINKS: M-VAST-SERIALIZE, M-VAST-META
// ROLE: RUNTIME
// MAP_MODE: EXPORTS
// END_MODULE_CONTRACT
//
// START_MODULE_MAP
// Serialize - Vast serializer adapter and namespace prefix
// END_MODULE_MAP
//
// START_CHANGE_SUMMARY
// LAST_CHANGE: v0.1.0 - Added initial GRACE contracts and module map for the first governed Vast wave.
// END_CHANGE_SUMMARY
package vast
//
//
//
//
import hrf.colmat._
import hrf.logger._
//
//
//
//

import hrf.serialize._

object Serialize extends Serializer {
    val gaming = vast.gaming

    // START_CONTRACT: writeFaction
    // PURPOSE: Serialize a Vast faction through the canonical Meta mapping.
    // INPUTS: { f: F - faction to serialize }
    // OUTPUTS: { String - stable faction identifier }
    // SIDE_EFFECTS: none
    // LINKS: M-VAST-SERIALIZE, M-VAST-META
    // END_CONTRACT: writeFaction
    def writeFaction(f : F) = Meta.writeFaction(f)

    // START_CONTRACT: parseFaction
    // PURPOSE: Parse a serialized Vast faction identifier through the canonical Meta mapping.
    // INPUTS: { s: String - serialized faction identifier }
    // OUTPUTS: { Option[F] - parsed faction when supported }
    // SIDE_EFFECTS: none
    // LINKS: M-VAST-SERIALIZE, M-VAST-META
    // END_CONTRACT: parseFaction
    def parseFaction(s : String) = Meta.parseFaction(s)

    val prefix = "vast."
}
