import React, { Component } from 'react'
import { connect } from 'react-redux'
import { fetchConsumersIfNeeded } from '../actions'
import ListDetailContainer from '../containers/ListDetailContainer'

class App extends Component {
  componentDidMount() {
    const { dispatch } = this.props
    dispatch(fetchConsumersIfNeeded())
  }

  render() {
    return (
      <div>
        <ListDetailContainer />
      </div>
    );
  }
}

function mapStateToProps(state) {
  return {};
}

export default connect(mapStateToProps)(App)
