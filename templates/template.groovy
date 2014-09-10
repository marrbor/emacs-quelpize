/*
 *   Copyright (c) Genetec Corporation. All Rights Reserved.
 */

package jp.survei.

import jp.survei.base.BusMod

import org.vertx.java.core.Future

import groovy.transform.CompileStatic
import groovy.transform.TypeCheckingMode

// use @CompileStatic(TypeCheckingMode.SKIP) for method includes dynamic contents.

@CompileStatic
class %file-without-ext% extends BusMod {

  def spec = [:]

  @Override def start(Future<Void> sr) {
    super.start()
    logger.info "Start %file-without-ext%"
    def confresult = chkconfig(config, spec)   // verify configuration.
    logger.debug "chkconfig returns ${confresult}."
    if (confresult) sr.setFailure(confresult) // something wrong.
    else {
      sr.setResult(null)
    }
  }

  @Override def stop() {
    logger.info "%file-without-ext% Stopped."
  }
}
