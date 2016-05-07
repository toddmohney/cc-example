import React, { PropTypes } from 'react'
import ConsumerTable from './ConsumerTable'
import ConsumerDetail from './ConsumerDetail'

const ListDetailView = ({ listData, onListItemClick, selectedListItem }) => (
  <div className="row">
    <div className="col-md-6">
      <ConsumerTable
        listData={listData}
        onListItemClick={onListItemClick}
        selectedListItem={selectedListItem}
      />
    </div>

    <div className="col-md-6">
      <ConsumerDetail
        consumer={selectedListItem}
      />
    </div>
  </div>
)

ListDetailView.propTypes = {
  listData: PropTypes.array.isRequired,
  selectedListItem: PropTypes.object,
  onListItemClick: PropTypes.func.isRequired
}

export default ListDetailView

