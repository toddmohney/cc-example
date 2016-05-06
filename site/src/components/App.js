import React, { Component } from 'react'
import { connect } from 'react-redux'
import { fetchConsumersIfNeeded } from '../actions'
import ListDetailView from './ListDetailView'

class App extends Component {
  componentDidMount() {
    const { dispatch } = this.props
    dispatch(fetchConsumersIfNeeded())
  }

  render() {
    return (
      <ListDetailView />
    );
  }
}

function mapStateToProps(state) {
  return {};
}

export default connect(mapStateToProps)(App)
