/*
 *   Copyright (c) Genetec Corporation. All Rights Reserved.
 */

package jp.survei.

import net.iperfecta.Ruisdael.Base.BusMod

import org.vertx.java.core.Future

class %file-without-ext% extends BusMod {

  def spec = [:]

  @Override def start(Future<Void> startedResult) {
    super.start()
    def confresult = chkconfig(config, spec)   // verify configuration.
    logger.debug "chkconfig returns ${ confresult}."
    if (confresult) startedResult.setFailure(confresult) // something wrong.
    else { 
      startedResult.setResult(null)
    }
  }
}
