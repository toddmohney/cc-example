import React from 'react'
import ConsumerList from './ConsumerList'
import ConsumerDetail from './ConsumerDetail'

const ListDetailView = () => (
  <div className="row">
    <div className="col-md-6">
      <ConsumerList />
    </div>

    <div className="col-md-6">
      <ConsumerDetail />
    </div>
  </div>
)

export default ListDetailView

