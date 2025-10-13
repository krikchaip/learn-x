import React from 'react'
import { connect } from 'react-redux'
import moment from 'moment'

import notification from 'modules/notification'

type Props = ReturnType<typeof mapStateToProps>

const mapStateToProps = (state: AppState) => ({
  notifications: notification.selectors.getNotifications(state)
})

export const Notifications = (props: Props) => {
  const { notifications } = props
  return (
    <div className="section">
      <div className="card z-depth-0">
        <div className="card-content">
          <span className="card-title">Notifications</span>
          <ul className="notifications">
            {notifications.map(noti => (
              <li key={noti.id}>
                <span className="pink-text">{noti.name} </span>
                <span>{noti.detail}</span>
                <div className="grey-text note-date">
                  {moment(noti.time).fromNow()}
                </div>
              </li>
            ))}
          </ul>
        </div>
      </div>
    </div>
  )
}

export default connect(mapStateToProps)(Notifications)
