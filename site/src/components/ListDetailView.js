import React, { PropTypes } from 'react'
import ConsumerList from './ConsumerList'
import ConsumerDetail from './ConsumerDetail'

const ListDetailView = ({ listData, onListItemClick }) => (
  <div className="row">
    <div className="col-md-6">
      <ConsumerList
        listData={listData}
        onListItemClick={onListItemClick}
      />
    </div>

    <div className="col-md-6">
      <ConsumerDetail />
    </div>
  </div>
)

ListDetailView.propTypes = {
  listData: PropTypes.array.isRequired,
  onListItemClick: PropTypes.func.isRequired
}

export default ListDetailView

