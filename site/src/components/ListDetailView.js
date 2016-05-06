import React, { PropTypes } from 'react'
import ConsumerList from './ConsumerList'
import ConsumerDetail from './ConsumerDetail'

const ListDetailView = ({ listData, onListItemClick, selectedListItem }) => (
  <div className="row">
    <div className="col-md-6">
      <ConsumerList
        listData={listData}
        onListItemClick={onListItemClick}
        selectedListItem={selectedListItem}
      />
    </div>

    <div className="col-md-6">
      <ConsumerDetail />
    </div>
  </div>
)

ListDetailView.propTypes = {
  listData: PropTypes.array.isRequired,
  selectedListItem: PropTypes.object.isRequired,
  onListItemClick: PropTypes.func.isRequired
}

export default ListDetailView

