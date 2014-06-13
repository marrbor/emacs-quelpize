/*
 *   Copyright (c) Genetec Corporation. All Rights Reserved.
 */

package jp.survei.

import net.iperfecta.Ruisdael.Base.BusMod

import org.vertx.java.core.Future

class %file-without-ext% extends BusMod {

  def spec = [:]

  @Override def start(Future<Void> sr) {
    super.start()
    def confresult = chkconfig(config, spec)   // verify configuration.
    logger.debug "chkconfig returns ${ confresult}."
    if (confresult) sr.setFailure(confresult) // something wrong.
    else {
      sr.setResult(null)
    }
  }

  @Override def stop() {
    logger.info "Stopped."
  }
}
