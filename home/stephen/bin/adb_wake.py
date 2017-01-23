#!/usr/bin/env monkeyrunner

from com.android.monkeyrunner import MonkeyRunner as m
import java.lang.RuntimeException

d = m.waitForConnection()
#d.wake()

isKeyguard = False
try: # The view server is only available on eng devices.
  isKeyguard = d.getHierarchyViewer().getFocusedWindowName() == u'Keyguard'
except java.lang.RuntimeException:
  # HACK: what's the preferred way to get the current activity?
  isKeyguard = d.shell('dumpsys window windows | grep -E "mCurrentFocus=.*Keyguard"').__len__() > 0

if isKeyguard is True:
  h = int(int(d.getProperty('display.height')) * 2/3.)
  w = int(d.getProperty('display.width'))
  d.drag((w/2, h), (w, h), .1, 100)
