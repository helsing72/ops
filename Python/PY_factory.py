from ops.Factory import BasicFactory

import PY

class PY_factory(BasicFactory):
	def __init__(self):
		super(PY_factory,self).__init__()
		self.addType("PY.XdrDebug",PY.XdrDebug)
		self.addType("PY.XdrSignedDebug",PY.XdrSignedDebug)