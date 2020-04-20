#include <stdlib.h>
#include <stdio.h>
#include "HsFFI.h"
#include <unistd.h>
#include "Rts.h"

// Copied from "stub.h" file

extern HsInt64 hsJsaddleProcessResult(HsStablePtr a1, HsBool a3, HsInt a4);
extern HsPtr hsJsaddleBufferMalloc(HsStablePtr a1, HsInt a2);
extern void setKeepCAFs(void);

HsStablePtr hsEnvPtr = NULL;

HsPtr jsaddleBufferMalloc(int size) {
  return hsJsaddleBufferMalloc(hsEnvPtr, size);
}

int jsaddleProcessResult (bool isSync, int dataLen) {
  int ret = hsJsaddleProcessResult (hsEnvPtr, isSync, dataLen);
  return ret;
}

// Does not return
void jsaddle_wasm_init(HsStablePtr p){
  hsEnvPtr = p;
  setKeepCAFs();
  _Exit(0);
}
